// Grade students' assembly with the C Call cases, from a Canvas bulk download.
//
//   npm run grade-cases -- <submissions> <cases-folder> [options]
//
// <submissions> is the submissions.zip that Canvas's "Download Submissions" gives, or the folder
// it was unzipped into. Canvas names each file <name>[_LATE]_<user id>_<submission id>_<file>;
// a student's files may be zips (read in memory, including zips inside them) or .s files.
// A file counts for question HWn_qx when its name is HWn_qx.s, ignoring folders, letter case and
// copy numbers such as "HWn_qx (1).s" or "HWn_qx-1.s".
//
// <cases-folder> holds the questions: each HWn_qx.ccall under it, with the HWn_qx.s starter
// beside it (its @ C: prototypes are used, so a student's edits to them do not matter).
//
// A question is worth its points x (cases passed / cases). A case passes when all its checks
// pass and the call has no calling-convention problem, exactly as check-cases and the panel judge it.
//
// Options:
//   --out <folder>          where to write the results (default: grades)
//   --points <file.json>    points per question, such as {"HW4_2a": 10, "HW4_2d": 20}
//   --default-points <n>    points for a question not in --points (default 10)
//   --gradebook <file.csv>  a Canvas gradebook export: also write canvas-import.csv for this column
//   --column <text>         the gradebook column to fill, such as "Homework 4" (part of its name)
//
// Results: grades.csv (a row per student, a column per question), feedback/<name>_<id>.txt
// (every case's result, for a submission comment) and, with --gradebook, canvas-import.csv,
// which Canvas's Grades > Import accepts. Its total is scaled to the column's points possible.
//
// Guide: docs/writing-cases.md

import fs from "fs";
import path from "path";
import zlib from "zlib";
import { prototypes, runCases, sourceLines } from "./cases-lib.mjs";

// ---------------------------------------------------------------- arguments

const argv = process.argv.slice(2);
const opts = { out: "grades", "default-points": "10" };
const positional = [];
for (let i = 0; i < argv.length; i++) {
    if (argv[i].startsWith("--")) opts[argv[i].slice(2)] = argv[++i];
    else positional.push(argv[i]);
}
const [subsPath, casesDir] = positional;
const usage = () => {
    console.error("usage: npm run grade-cases -- <submissions.zip or folder> <cases-folder> [--out folder] " +
        "[--points file.json] [--default-points n] [--gradebook export.csv --column text]");
    process.exit(2);
};
if (!subsPath || !casesDir || !fs.existsSync(subsPath) || !fs.existsSync(casesDir)) usage();
if (Boolean(opts.gradebook) !== Boolean(opts.column)) usage();

// ---------------------------------------------------------------- zip files, read in memory

const MAX_ENTRY = 64 * 1024 * 1024; // a larger entry is skipped: no source file is that big

/** the entries of a zip: [{ name, data: Buffer }], skipping folders and encrypted entries */
const readZip = (buf) => {
    let eocd = -1;
    for (let i = buf.length - 22; i >= Math.max(0, buf.length - 22 - 65535); i--)
        if (buf.readUInt32LE(i) === 0x06054b50) { eocd = i; break; }
    if (eocd < 0) throw new Error("not a zip file");
    let count = buf.readUInt16LE(eocd + 10), at = buf.readUInt32LE(eocd + 16);
    if (count === 0xFFFF || at === 0xFFFFFFFF) { // ZIP64: the real values are in the ZIP64 end record
        const loc = eocd - 20;
        if (loc < 0 || buf.readUInt32LE(loc) !== 0x07064b50) throw new Error("damaged ZIP64 file");
        const rec = Number(buf.readBigUInt64LE(loc + 8));
        count = Number(buf.readBigUInt64LE(rec + 32));
        at = Number(buf.readBigUInt64LE(rec + 48));
    }
    const entries = [];
    for (let n = 0; n < count; n++) {
        if (buf.readUInt32LE(at) !== 0x02014b50) throw new Error("damaged zip directory");
        const flags = buf.readUInt16LE(at + 8), method = buf.readUInt16LE(at + 10);
        let csize = buf.readUInt32LE(at + 20), usize = buf.readUInt32LE(at + 24);
        const nlen = buf.readUInt16LE(at + 28), xlen = buf.readUInt16LE(at + 30), clen = buf.readUInt16LE(at + 32);
        let local = buf.readUInt32LE(at + 42);
        const name = buf.subarray(at + 46, at + 46 + nlen).toString(flags & 0x800 ? "utf8" : "latin1");
        // ZIP64 sizes and offset in the extra field
        for (let x = at + 46 + nlen; x < at + 46 + nlen + xlen;) {
            const id = buf.readUInt16LE(x), len = buf.readUInt16LE(x + 2);
            if (id === 1) {
                let p = x + 4;
                if (usize === 0xFFFFFFFF) { usize = Number(buf.readBigUInt64LE(p)); p += 8; }
                if (csize === 0xFFFFFFFF) { csize = Number(buf.readBigUInt64LE(p)); p += 8; }
                if (local === 0xFFFFFFFF) local = Number(buf.readBigUInt64LE(p));
            }
            x += 4 + len;
        }
        at += 46 + nlen + xlen + clen;
        if (name.endsWith("/") || flags & 1 || usize > MAX_ENTRY) continue;
        const start = local + 30 + buf.readUInt16LE(local + 26) + buf.readUInt16LE(local + 28);
        const raw = buf.subarray(start, start + csize);
        let data;
        try {
            data = method === 0 ? Buffer.from(raw) : method === 8 ? zlib.inflateRawSync(raw, { maxOutputLength: MAX_ENTRY }) : null;
        } catch { data = null; }
        entries.push({ name, data }); // data null: compressed in a way this reader does not know
    }
    return entries;
};

const isZip = (buf) => buf.length >= 4 && buf.readUInt32LE(0) === 0x04034b50;

/** every file in a submitted file, looking inside zips (two levels deep): [{ name, data }] */
const expand = (name, data, depth = 0) => {
    if (!data || !isZip(data) || depth > 2) return [{ name, data }];
    let entries;
    try { entries = readZip(data); } catch (e) { return [{ name, data: null, error: `${name}: ${e.message}` }]; }
    return entries
        .filter((e) => !e.name.startsWith("__MACOSX/") && !path.posix.basename(e.name).startsWith("._"))
        .flatMap((e) => expand(`${name}/${e.name}`, e.data, depth + 1));
};

// ---------------------------------------------------------------- the questions

const findCases = (dir) =>
    fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
        const p = path.join(dir, e.name);
        return e.isDirectory() ? findCases(p) : e.name.endsWith(".ccall") ? [p] : [];
    }).sort();

const pointsFor = opts.points ? JSON.parse(fs.readFileSync(opts.points, "utf8")) : {};
const questions = findCases(casesDir).map((ccPath) => {
    const id = path.basename(ccPath, ".ccall");
    const starter = ccPath.replace(/\.ccall$/, ".s");
    const cc = JSON.parse(fs.readFileSync(ccPath, "utf8"));
    const cases = Object.values(cc.functions ?? {}).reduce((n, f) => n + (f.cases ?? []).length, 0);
    return {
        id, cc, cases,
        protos: fs.existsSync(starter) ? prototypes(sourceLines(fs.readFileSync(starter))).protos : null,
        points: Number(pointsFor[id] ?? opts["default-points"]),
    };
});
if (questions.length === 0) { console.error(`no .ccall files found in ${casesDir}`); process.exit(2); }
for (const id of Object.keys(pointsFor))
    if (!questions.some((q) => q.id === id)) console.error(`warning: --points names ${id}, which has no .ccall`);
const maxPoints = questions.reduce((n, q) => n + q.points, 0);
const questionOf = (file) => {
    const m = /^(hw\d+_\d+[a-z])(?:\s*\(\d+\)|-\d+)?\.s$/i.exec(path.posix.basename(file.replace(/\\/g, "/")));
    return m ? questions.find((q) => q.id.toLowerCase() === m[1].toLowerCase()) : undefined;
};

// ---------------------------------------------------------------- the submissions, by student

/** Canvas's name for a submitted file: <name>[_LATE]_<user id>_<submission id>_<file> */
const canvasName = (file) => {
    const m = /^(.*?)(_LATE)?_(\d+)_(\d+)_(.+)$/.exec(file);
    return m ? { name: m[1], late: Boolean(m[2]), user: m[3], file: m[5] } : { name: file.replace(/\.[^.]*$/, ""), late: false, user: "", file };
};

const top = fs.statSync(subsPath).isDirectory()
    ? fs.readdirSync(subsPath).filter((f) => fs.statSync(path.join(subsPath, f)).isFile())
        .map((f) => ({ name: f, data: fs.readFileSync(path.join(subsPath, f)) }))
    : readZip(fs.readFileSync(subsPath)).filter((e) => !e.name.startsWith("__MACOSX/"))
        .map((e) => ({ name: path.posix.basename(e.name), data: e.data }));

const students = new Map();
for (const f of top.sort((a, b) => a.name.localeCompare(b.name))) {
    const c = canvasName(f.name);
    const key = c.user || c.name;
    if (!students.has(key)) students.set(key, { name: c.name, user: c.user, late: false, files: [] });
    const s = students.get(key);
    s.late ||= c.late;
    s.files.push(...expand(c.file, f.data));
}

// ---------------------------------------------------------------- grading

const round = (x) => Math.round(x * 100) / 100;
const results = [];
for (const s of students.values()) {
    const found = new Map(), ignored = [], unreadable = [];
    for (const f of s.files) {
        if (f.error) { unreadable.push(f.error); continue; }
        const q = questionOf(f.name);
        if (!q && /\.(rar|7z|tar|gz|tgz|bz2|xz)$/i.test(f.name)) { unreadable.push(`${f.name}: only .zip archives can be read`); continue; }
        if (!q) { ignored.push(f.name); continue; }
        if (!f.data) { unreadable.push(`${f.name}: compressed in a way the grader cannot read`); continue; }
        if (!found.has(q.id)) found.set(q.id, []);
        found.get(q.id).push(f);
    }
    const perQuestion = questions.map((q) => {
        const files = (found.get(q.id) ?? []).sort((a, b) => a.name.split("/").length - b.name.split("/").length || a.name.localeCompare(b.name));
        if (files.length === 0) return { q, file: null, passed: 0, score: 0, cases: [] };
        const lines = sourceLines(files[0].data);
        const cases = runCases(q.cc, lines, q.protos ?? prototypes(lines).protos);
        const passed = cases.filter((r) => r.ok).length;
        return { q, file: files[0].name, others: files.slice(1).map((f) => f.name), passed, cases, score: round(q.points * passed / (q.cases || 1)) };
    });
    const total = round(perQuestion.reduce((n, r) => n + r.score, 0));
    results.push({ s, perQuestion, total, ignored, unreadable });
    console.log(`${s.name.padEnd(28)} ${String(total).padStart(7)} / ${maxPoints}` +
        `  (${perQuestion.filter((r) => r.file).length}/${questions.length} questions found${s.late ? ", late" : ""})`);
}

// ---------------------------------------------------------------- results

fs.mkdirSync(path.join(opts.out, "feedback"), { recursive: true });
const csvCell = (v) => /[",\n]/.test(String(v)) ? '"' + String(v).replace(/"/g, '""') + '"' : String(v);
const csv = (rows) => rows.map((r) => r.map(csvCell).join(",")).join("\r\n") + "\r\n";

fs.writeFileSync(path.join(opts.out, "grades.csv"), csv([
    ["Student", "Canvas user id", "Late", ...questions.map((q) => `${q.id} (${q.points})`), `Total (${maxPoints})`],
    ...results.map(({ s, perQuestion, total }) => [s.name, s.user, s.late ? "late" : "", ...perQuestion.map((r) => r.file ? r.score : ""), total]),
]));

for (const { s, perQuestion, total, ignored, unreadable } of results) {
    const out = [`${s.name}${s.user ? ` (Canvas user ${s.user})` : ""}${s.late ? ", late" : ""}`,
        `Total: ${total} / ${maxPoints}`, ""];
    for (const r of perQuestion) {
        out.push(`${r.q.id}: ${r.score} / ${r.q.points} points, ${r.passed} of ${r.q.cases} cases passed`);
        if (!r.file) { out.push(`  no file named ${r.q.id}.s was found`, ""); continue; }
        out.push(`  graded ${r.file}`);
        for (const o of r.others ?? []) out.push(`  also found ${o}, which was not graded`);
        for (const c of r.cases) {
            out.push(`  ${c.ok ? "✔" : "✘"} ${c.fn}: ${c.name}`);
            if (!c.ok) for (const d of c.details) out.push(`      ${d}`);
        }
        out.push("");
    }
    if (ignored.length) out.push("Files that are not a question's .s file (not graded):", ...ignored.map((f) => `  ${f}`), "");
    if (unreadable.length) out.push("Files that could not be read:", ...unreadable.map((f) => `  ${f}`), "");
    const file = `${s.name}${s.user ? "_" + s.user : ""}.txt`.replace(/[^\w.-]+/g, "_");
    fs.writeFileSync(path.join(opts.out, "feedback", file), out.join("\r\n"));
}

// ---------------------------------------------------------------- Canvas gradebook import

/** rows of a CSV file */
const parseCsv = (text) => {
    const rows = []; let row = [], cell = "", quoted = false;
    for (let i = 0; i < text.length; i++) {
        const ch = text[i];
        if (quoted) {
            if (ch === '"' && text[i + 1] === '"') { cell += '"'; i++; }
            else if (ch === '"') quoted = false;
            else cell += ch;
        } else if (ch === '"') quoted = true;
        else if (ch === ",") { row.push(cell); cell = ""; }
        else if (ch === "\n" || ch === "\r") {
            if (ch === "\r" && text[i + 1] === "\n") i++;
            row.push(cell); rows.push(row); row = []; cell = "";
        } else cell += ch;
    }
    if (cell !== "" || row.length) { row.push(cell); rows.push(row); }
    return rows;
};

if (opts.gradebook) {
    const rows = parseCsv(fs.readFileSync(opts.gradebook, "utf8").replace(/^﻿/, ""));
    const head = rows[0];
    const matches = head.map((h, i) => [h, i]).filter(([h]) => h.toLowerCase().includes(opts.column.toLowerCase()));
    if (matches.length !== 1) {
        console.error(matches.length ? `"${opts.column}" matches several gradebook columns: ${matches.map(([h]) => h).join("; ")}`
            : `no gradebook column contains "${opts.column}"`);
        process.exit(2);
    }
    const col = matches[0][1];
    const idCol = head.indexOf("ID");
    if (idCol < 0) { console.error("the gradebook has no ID column: use a Canvas gradebook export"); process.exit(2); }
    const keep = head.map((h, i) => i).filter((i) => ["Student", "ID", "SIS User ID", "SIS Login ID", "Integration ID", "Section"].includes(head[i]));
    const possibleRow = rows.find((r) => (r[0] ?? "").trim() === "Points Possible");
    const possible = possibleRow ? Number(possibleRow[col]) : NaN;
    const scale = possible > 0 ? possible / maxPoints : 1;
    const byUser = new Map(results.filter((r) => r.s.user).map((r) => [r.s.user, r]));
    const used = new Set();
    const out = [[...keep.map((i) => head[i]), head[col]]];
    for (const r of rows.slice(1)) {
        if (r.length < 2) continue;
        const g = byUser.get(r[idCol]);
        if (g) used.add(r[idCol]);
        out.push([...keep.map((i) => r[i] ?? ""), g ? round(g.total * scale) : (r[col] ?? "")]);
    }
    fs.writeFileSync(path.join(opts.out, "canvas-import.csv"), csv(out));
    console.log(`\ncanvas-import.csv fills "${head[col]}"` +
        (scale !== 1 ? `, scaled from ${maxPoints} to its ${possible} points possible` : "") +
        "; students without a submission keep their current grade.");
    const missing = results.filter((r) => !r.s.user || !used.has(r.s.user));
    if (missing.length) console.log(`Not in the gradebook, so not in canvas-import.csv: ${missing.map((r) => r.s.name).join(", ")}`);
}

console.log(`\n${results.length} students graded out of ${maxPoints}. Results are in ${path.resolve(opts.out)}`);
