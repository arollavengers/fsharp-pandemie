#r "packages/FAKE.3.34.7/tools/FakeLib.dll"
open Fake 

//RestorePackages()

// Output Directories
let buildDir  = "./build/"
let testDir   = "./test/"
let deployDir = "./deploy/"

// -------------------------------------------------------------------
// Targets
// -------------------------------------------------------------------

Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "BuildApp" (fun _ ->
   !! "core/**/*.fsproj"
     |> MSBuildRelease buildDir "Build"
     |> Log "AppBuild-Output: "
)

Target "CleanTest" (fun _ ->
    CleanDir testDir
)

Target "BuildTest" (fun _ ->
    !! "core.test/**/*.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->  
    trace ""
    trace " --- Testing app --- "
    trace ""
    !! (testDir + "/core.test.dll")
        |> NUnit (fun p -> 
            {p with
                DisableShadowCopy = true; 
                OutputFile = testDir + "TestResults.xml"})
)

Target "Deploy" (fun () -> trace " --- Deploying app --- ")


"Clean"
  ==> "BuildApp"

"CleanTest"
  ==> "BuildTest"
  ==> "Test"

Dependencies "Deploy" ["BuildApp"; "Test"]

RunTargetOrDefault "Deploy"
