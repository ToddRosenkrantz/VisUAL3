// Run the C Call cases in .ccall files against assembly code, without the app.
//
//   npm run check-cases -- <cases-folder> [solutions-folder]
//
// Every .ccall file under <cases-folder> is checked. The code under test is the .s file with
// the same relative path under [solutions-folder] if one is given, or else the .s beside the
// .ccall. Each case is run as the C Call panel runs it; a case passes when all its checks pass
// and the call has no calling-convention problems. The exit code is 1 if any case fails.
//
// It uses the emulator as compiled by `npm run fable` (or `npm run build`).
// Guide: docs/writing-cases.md

import fs from "fs";
import path from "path";
import { fileURLToPath, pathToFileURL } from "url";

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), "..");
const emu = path.join(root, "build", "renderer", "Emulator");
if (!fs.existsSync(path.join(emu, "CCall.js"))) {
    console.error("The emulator is not built: run `npm run fable` (or `npm run build`) first.");
    process.exit(2);
}
const load = (p) => import(pathToFileURL(path.join(root, "build", "renderer", p)).href);
const { ofArray, toArray } = await load("fable_modules/fable-library-js.4.29.0/List.js");
const { ofArray: mapOfArray } = await load("fable_modules/fable-library-js.4.29.0/Map.js");
const { comparePrimitives } = await load("fable_modules/fable-library-js.4.29.0/Util.js");
const { findPrototypes, parseArgText, callFunction, checkExpectations, Expectation } = await load("Emulator/CCall.js");
const { reLoadProgram } = await load("Emulator/ExecutionTop.js");

const [casesDir, solutionsDir] = process.argv.slice(2);
if (!casesDir) {
    console.error("usage: npm run check-cases -- <cases-folder> [solutions-folder]");
    process.exit(2);
}

/** all .ccall files under dir, sorted */
const findCases = (dir) =>
    fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
        const p = path.join(dir, e.name);
        return e.isDirectory() ? findCases(p) : e.name.endsWith(".ccall") ? [p] : [];
    }).sort();

/** The text CCall.parseArgText takes for a saved argument of a parameter of type t, as
 *  CCallView.argOf builds it (src/Renderer/CCallView.fs): keep the two in step. The panel saves
 *  every argument as {kind, text}; the kind matters only for pointers. */
const CPtr = 2, CIntT = 1, CBool = 4; // CCall.CType and CInt case tags
const argText = (a, t) => {
    if (a === undefined || a === null) return "";
    const text = typeof a === "object" ? String(a.text ?? "") : String(a);
    if (t.tag !== CPtr) // a number: _Bool is 1 when given as 1, otherwise 0
        return t.tag === CIntT && t.fields[0].tag === CBool ? (text.trim() === "1" ? "1" : "0") : text.trim();
    if (typeof a !== "object") return text;
    switch (a.kind) {
        case "string": return text.trim().startsWith('"') ? text : '"' + text.replace(/"/g, '\\"') + '"';
        case "output": return "[" + text.trim() + "]";
        case "address": return text.trim();
        default: { // array: braces are optional in the panel
            const s = text.trim();
            return s.startsWith("{") || /\]\s*$/.test(s) ? s : "{" + s + "}";
        }
    }
};

let passed = 0, failed = 0;
const files = fs.existsSync(casesDir) ? findCases(casesDir) : [];
if (files.length === 0) {
    console.error(`no .ccall files found in ${casesDir}`);
    process.exit(2);
}
for (const ccPath of files) {
    const rel = path.relative(casesDir, ccPath).replace(/\.ccall$/, ".s");
    const sPath = solutionsDir ? path.join(solutionsDir, rel) : ccPath.replace(/\.ccall$/, ".s");
    const label = rel.replace(/\.s$/, "");
    const say = (fn, name, verdict) => console.log(`${label}  ${fn}  ${name}  ${verdict}`);
    if (!fs.existsSync(sPath)) { say("-", "-", `FAIL no code: ${sPath} not found`); failed++; continue; }
    let cc;
    try { cc = JSON.parse(fs.readFileSync(ccPath, "utf8")); }
    catch (e) { say("-", "-", `FAIL ${path.basename(ccPath)} is not valid JSON: ${e.message}`); failed++; continue; }
    // the lines as the editor holds them (CR LF files are fine)
    const lines = ofArray(fs.readFileSync(sPath, "utf8").replace(/\r/g, "").split("\n"));
    const lim = reLoadProgram(lines);
    const protos = new Map();
    for (const r of toArray(findPrototypes(lines))) {
        if (r.tag === 0) protos.set(r.fields[0].FName, r.fields[0]);
        else { say("-", "-", `FAIL @ C: line ${r.fields[0][0] + 1}: ${r.fields[0][1]}`); failed++; }
    }
    for (const [fn, fc] of Object.entries(cc.functions ?? {})) {
        const proto = protos.get(fn);
        for (const c of fc.cases ?? []) {
            const name = c.name ?? "(unnamed)";
            if (!proto) { say(fn, name, `FAIL no @ C: prototype for ${fn} in ${path.basename(sPath)}`); failed++; continue; }
            const args = toArray(proto.Params).map((p) => parseArgText(p.PType, argText((c.args ?? {})[p.PName], p.PType)));
            const bad = args.filter((a) => a.tag !== 0).map((a) => a.fields[0]);
            if (bad.length) { say(fn, name, "FAIL argument: " + bad.join("; ")); failed++; continue; }
            const r = callFunction(lim, lines, proto, ofArray(args.map((a) => a.fields[0])), 1000000n);
            if (r.tag !== 0) { say(fn, name, "FAIL " + toArray(r.fields[0]).join("; ")); failed++; continue; }
            const [setup, res] = r.fields[0];
            const expect = c.expect ?? {};
            const after = Object.entries(expect).filter(([k]) => k !== "return").map(([k, v]) => [k, String(v)]);
            const ex = new Expectation(String(expect.return ?? ""), mapOfArray(after, { Compare: comparePrimitives }));
            const checks = toArray(checkExpectations(setup, res, ex));
            const problems = toArray(res.Problems).map(String);
            const ok = checks.length > 0 && checks.every((k) => k.Passed) && problems.length === 0;
            const details = checks.map((k) => `${k.Passed ? "✔" : "✘"} ${k.What}: ${k.Detail}`)
                .concat(problems.map((p) => "⚠ " + p))
                .concat(checks.length === 0 ? ["(the case expects nothing, so nothing was checked)"] : []);
            say(fn, name, (ok ? "ok    " : "FAIL  ") + details.join(" | "));
            ok ? passed++ : failed++;
        }
    }
}
console.log(`\n${passed} passed, ${failed} failed`);
process.exit(failed ? 1 : 0);
