// Bundle Fable output for Electron and copy browser libraries into app/js.
// Run after `dotnet fable` has written build/main and build/renderer (see package.json "fable").
import * as esbuild from "esbuild";
import { cpSync, existsSync, mkdirSync, rmSync } from "node:fs";

const external = ["electron", "@electron/remote", "@electron/remote/main"];

// Electron main process
await esbuild.build({
  entryPoints: ["build/main/Main.js"],
  outfile: "out/main.js",
  bundle: true,
  platform: "node",
  format: "cjs",
  target: "node22",
  external,
  sourcemap: true,
  logLevel: "warning",
});

// Renderer: a classic script; nodeIntegration provides require() for node and electron modules
if (existsSync("build/renderer/Renderer.js")) {
  await esbuild.build({
    entryPoints: ["build/renderer/Renderer.js"],
    outfile: "app/js/renderer.js",
    bundle: true,
    platform: "node",
    format: "iife",
    target: "chrome130",
    external,
    sourcemap: true,
    logLevel: "warning",
  });
}

// Browser libraries loaded by app/index.html
mkdirSync("app/js", { recursive: true });
rmSync("app/js/vs", { recursive: true, force: true });
cpSync("node_modules/monaco-editor/min/vs", "app/js/vs", { recursive: true });
cpSync("node_modules/tippy.js/dist/tippy-bundle.umd.min.js", "app/js/tippy.min.js");
cpSync("node_modules/@popperjs/core/dist/umd/popper.min.js", "app/js/popper.min.js");
console.log("bundle complete");
