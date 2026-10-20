// Zip packaged apps in dist/ for distribution: dist/VisUAL3-<platform>-<arch>-<version>.zip
// usage: node zip-dist.mjs [platform ...]   (default: every dist/VisUAL3-* folder)
// Uses `zip -y` so the macOS bundle keeps its symbolic links and executables keep their permissions;
// on a Windows build machine falls back to PowerShell Compress-Archive.
import { readdirSync, readFileSync, rmSync, statSync } from "node:fs";
import { execFileSync } from "node:child_process";

const version = JSON.parse(readFileSync("package.json", "utf8")).version;
const platforms = process.argv.slice(2);
const folders = readdirSync("dist").filter(name =>
  name.startsWith("VisUAL3-") && statSync(`dist/${name}`).isDirectory() &&
  (platforms.length === 0 || platforms.some(p => name.startsWith(`VisUAL3-${p}-`))));

if (folders.length === 0) {
  console.error(`no packaged apps in dist/ for: ${platforms.join(", ") || "any platform"}`);
  process.exit(1);
}
for (const folder of folders) {
  const zipName = `${folder}-${version}.zip`;
  rmSync(`dist/${zipName}`, { force: true });
  if (process.platform === "win32") {
    execFileSync("powershell", ["-NoProfile", "-Command",
      `Compress-Archive -Path '${folder}' -DestinationPath '${zipName}'`], { cwd: "dist", stdio: "inherit" });
  } else {
    execFileSync("zip", ["-r", "-y", "-q", zipName, folder], { cwd: "dist", stdio: "inherit" });
  }
  console.log(`dist/${zipName}  ${(statSync(`dist/${zipName}`).size / 1e6).toFixed(0)} MB`);
}
