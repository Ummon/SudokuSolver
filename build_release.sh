#!/usr/bin/env bash

./dependencies.sh

chmod u+x packages/FAKE/tools/FAKE.exe

packages/FAKE/tools/FAKE.exe build.fsx release
