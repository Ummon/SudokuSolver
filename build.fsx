#!/usr/bin/fsharpi

#I "packages/FAKE/tools/"
#r @"packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r @"packages/FAKE/tools/FakeLib.dll"

open System.Diagnostics
open Fake
open Fake.EnvironmentHelper

let buildDirDebug = "./build/Debug/"
let buildDirRelease = "./build/Release/"

Target "Clean" (fun _ ->
    trace "Cleaning..."
    CleanDir buildDirDebug
    CleanDir buildDirRelease
)

Target "Debug" (fun _ ->
    trace "Building in Debug mode..."
    !! "**/*.fsproj" |> MSBuildDebug buildDirDebug "Build" |> Log "Debug-Output:"
)

Target "Release" (fun _ ->
    trace "Building in Release mode..."
    !! "**/*.fsproj" |> MSBuildRelease buildDirRelease "Build" |> Log "Release-Output:"
)

Target "Deploy" (fun _ ->
    trace "Deployement..."
)

"Clean" ==> "Release"
"Release" ==> "Deploy"

RunTargetOrDefault "Debug"
