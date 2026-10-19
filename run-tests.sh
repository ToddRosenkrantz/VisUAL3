#!/usr/bin/env bash
# Run emulator tests on .NET and as Fable-compiled JS under Node.
set -e
cd "$(dirname "$0")"
echo "== .NET =="
dotnet run --project tests/EmulatorTests
echo "== Fable/Node =="
dotnet fable tests/EmulatorTests -o build/tests --silent
echo '{"type":"module"}' > build/tests/package.json
node build/tests/Tests.js
