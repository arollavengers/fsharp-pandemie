#r "packages/FAKE.3.34.7/tools/FakeLib.dll"
open Fake 

// Directories
let buildDir  = "./build/"

let appDir   = "./core"
let testDir   = "./core.test"
let deployDir = "./deploy/"


// Targets

Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "BuildApp" (fun _ ->
   !! "core/**/*.fsproj"
     |> MSBuildRelease buildDir "Build"
     |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    !! "core.test/**/*.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->  
    !! (testDir + "/NUnit.Test.*.dll")
        |> NUnit (fun p -> 
            {p with
                DisableShadowCopy = true; 
                OutputFile = testDir + "TestResults.xml"})
)

Target "Deploy" (fun () -> trace " --- Deploying app --- ")

"Clean"
  ==> "BuildApp"
  ==> "BuildTest"
  ==> "Test"
  ==> "Deploy"


RunTargetOrDefault "Deploy"
