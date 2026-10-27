// The version Windows records in VisUAL3.exe, which must be at most four numbers:
// package.json's 3.0.0-dev.9.0.1 becomes 3.0.0.9 (the dev build's first number, or 0).
// The app itself shows the full version (src/Renderer/Refs.fs).

import fs from "fs";

const v = JSON.parse(fs.readFileSync(new URL("../package.json", import.meta.url), "utf8")).version;
const [core, pre = ""] = v.split("-");
console.log(`${core}.${(pre.match(/\d+/) ?? ["0"])[0]}`);
