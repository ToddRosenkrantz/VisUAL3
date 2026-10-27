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
import { prototypes, runCases, sourceLines } from "./cases-lib.mjs";

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
    const lines = sourceLines(fs.readFileSync(sPath));
    const { protos, errors } = prototypes(lines);
    for (const e of errors) { say("-", "-", "FAIL " + e); failed++; }
    for (const r of runCases(cc, lines, protos)) {
        say(r.fn, r.name, (r.ok ? "ok    " : "FAIL  ") + r.details.join(" | "));
        r.ok ? passed++ : failed++;
    }
}
console.log(`\n${passed} passed, ${failed} failed`);
process.exit(failed ? 1 : 0);
