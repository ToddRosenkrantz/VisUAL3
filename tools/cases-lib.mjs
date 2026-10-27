// Run the cases of a .ccall file against assembly source, as the C Call panel runs them.
// Shared by check-cases.mjs and grade-cases.mjs. It uses the emulator as compiled by
// `npm run fable` (or `npm run build`).

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

/** source text as the editor holds it: lines, with CR LF, a UTF-8 BOM or UTF-16 (Notepad) handled */
export const sourceLines = (buf) => {
    let text;
    if (buf[0] === 0xFF && buf[1] === 0xFE) text = buf.subarray(2).toString("utf16le");
    else if (buf[0] === 0xFE && buf[1] === 0xFF) text = Buffer.from(buf.subarray(2)).swap16().toString("utf16le");
    else text = buf.toString("utf8").replace(/^﻿/, "");
    return text.replace(/\r/g, "").split("\n");
};

/** The `@ C:` prototypes in lines: a Map from function name, and the error messages */
export const prototypes = (lines) => {
    const protos = new Map(), errors = [];
    for (const r of toArray(findPrototypes(ofArray(lines)))) {
        if (r.tag === 0) protos.set(r.fields[0].FName, r.fields[0]);
        else errors.push(`@ C: line ${r.fields[0][0] + 1}: ${r.fields[0][1]}`);
    }
    return { protos, errors };
};

/** Run every case in cc (a parsed .ccall) against the code in lines. The prototypes come from
 *  protos (by default those in lines). Returns one result per case:
 *  { fn, name, ok, details: [string] } */
export const runCases = (cc, lines, protos = prototypes(lines).protos, steps = 1000000n) => {
    const results = [];
    const list = ofArray(lines);
    let lim;
    try { lim = reLoadProgram(list); } catch (e) { lim = undefined; }
    for (const [fn, fc] of Object.entries(cc.functions ?? {})) {
        const proto = protos.get(fn);
        for (const c of fc.cases ?? []) {
            const name = c.name ?? "(unnamed)";
            const fail = (msg) => results.push({ fn, name, ok: false, details: [msg] });
            if (!proto) { fail(`no @ C: prototype for ${fn}`); continue; }
            if (!lim) { fail("the code could not be loaded"); continue; }
            try {
                const args = toArray(proto.Params).map((p) => parseArgText(p.PType, argText((c.args ?? {})[p.PName], p.PType)));
                const bad = args.filter((a) => a.tag !== 0).map((a) => a.fields[0]);
                if (bad.length) { fail("argument: " + bad.join("; ")); continue; }
                const r = callFunction(lim, list, proto, ofArray(args.map((a) => a.fields[0])), steps);
                if (r.tag !== 0) { fail(toArray(r.fields[0]).join("; ")); continue; }
                const [setup, res] = r.fields[0];
                const expect = c.expect ?? {};
                const after = Object.entries(expect).filter(([k]) => k !== "return").map(([k, v]) => [k, String(v)]);
                const ex = new Expectation(String(expect.return ?? ""), mapOfArray(after, { Compare: comparePrimitives }));
                const checks = toArray(checkExpectations(setup, res, ex));
                const problems = toArray(res.Problems).map(String);
                results.push({
                    fn, name,
                    ok: checks.length > 0 && checks.every((k) => k.Passed) && problems.length === 0,
                    details: checks.map((k) => `${k.Passed ? "✔" : "✘"} ${k.What}: ${k.Detail}`)
                        .concat(problems.map((p) => "⚠ " + p))
                        .concat(checks.length === 0 ? ["(the case expects nothing, so nothing was checked)"] : []),
                });
            } catch (e) {
                fail(`the emulator stopped: ${e.message ?? e}`);
            }
        }
    }
    return results;
};
