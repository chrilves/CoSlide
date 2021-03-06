lazy val commonScalacOptions = Seq(
    "-deprecation"                                     // Emit warning and location for usages of deprecated APIs.
  , "-explaintypes"                                    // Explain type errors in more detail.
  , "-feature"                                         // Emit warning and location for usages of features that should be imported explicitly.
  , "-language:_"                                      // Enable or disable language features: `_' for all, `-language:help' to list choices.
  , "-opt:l:inline"                                    // Enable optimizations: `_' for all, `-opt:help' to list choices.
  , "-opt-inline-from:**"                              // Patterns for classfile names from which to allow inlining, `help` for details.
  , "-opt-warnings:_"                                  // Enable optimizer warnings: `_' for all, `-opt-warnings:help' to list choices.
  , "-unchecked"                                       // Enable additional warnings where generated code depends on assumptions.
  , "-Xcheckinit"                                      // Wrap field accessors to throw an exception on uninitialized access.
  , "-Xdev"                                            // Indicates user is a developer - issue warnings about anything which seems amiss
  , "-Xexperimental"                                   // Enable experimental extensions.
//  , "-Xfatal-warnings"                                 // Fail the compilation if there are any warnings.
  , "-Xfuture"                                         // Turn on future language features.
//  , "-Xgenerate-phase-graph",  "target/out-graph.dot"  // Generate the phase graphs (outputs .dot files) to fileX.dot.
  , "-Xlint:_"                                         // Enable or disable specific warnings: `_' for all, `-Xlint:help' to list choices.
  , "-Xverify"                                         // Verify generic signatures in generated bytecode.
  , "-Ybreak-cycles"                                   // Attempt to break cycles encountered during typing
//  , "-Ygen-asmp", "target/asmp"                        // Generate a parallel output directory of .asmp files (ie ASM Textifier output).
  , "-Yinfer-argument-types"                           // Infer types for arguments of overridden methods.
  , "-Yno-adapted-args"                                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  , "-Yopt-inline-heuristics:everything"               // Set the heuristics for inlining decisions. Choices: (at-inline-annotated,everything,default), default: default.
  , "-Ypartial-unification"                            // Enable partial unification in type constructor inference
  , "-Yrecursion", "0"                                 // Set recursion depth used when locking symbols.
  , "-Ywarn-adapted-args"                              // Warn if an argument list is modified to match the receiver.
  , "-Ywarn-dead-code"                                 // Warn when dead code is identified.
  , "-Ywarn-extra-implicit"                            // Warn when more than one implicit parameter section is defined.
  , "-Ywarn-inaccessible"                              // Warn about inaccessible types in method signatures.
  , "-Ywarn-infer-any"                                 // Warn when a type argument is inferred to be `Any`.
  , "-Ywarn-nullary-override"                          // Warn when non-nullary `def f()' overrides nullary `def f'.
  , "-Ywarn-nullary-unit"                              // Warn when nullary methods return Unit.
  , "-Ywarn-numeric-widen"                             // Warn when numerics are widened.
  , "-Ywarn-unused:_"                                  // Enable or disable specific `unused' warnings: `_' for all, `-Ywarn-unused:help' to list choices.
  , "-Ywarn-unused-import"                             // Warn when imports are unused.
  , "-Ywarn-value-discard"                             // Warn when non-Unit expression results are unused.
)

// Option for the Splain compiler plugin
val splainOptions = List("all", "tree")

lazy val commonSettings = Seq(
  organization := "coslides",
  scalaVersion := "2.12.3",
  scalacOptions := commonScalacOptions,

  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
    compilerPlugin("io.tryp" %% "splain" % "0.2.6"),
    compilerPlugin("org.wartremover" %% "wartremover" % "1.2.1"),
    "com.chuusai" %%% "shapeless" % "2.3.2",
    "org.typelevel" %%% "cats-mtl-core" % "0.0.2",
    "com.lihaoyi" %%% "scalatags" % "0.6.7",
    "com.github.japgolly.scalacss" %%% "core" % "0.5.3",
    "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
  ) ++ List("laws", "free").map(pkg => "org.typelevel" %%% s"cats-$pkg" % "1.0.0-MF"),

  wartremoverErrors ++= Warts.allBut(Wart.DefaultArguments, Wart.ImplicitParameter),
  scalacOptions ++= splainOptions.map(opt => s"-P:splain:$opt")
)

lazy val coslide =
  crossProject.crossType(CrossType.Full)
              .in(file("."))
              .settings(commonSettings: _*)
              .enablePlugins(ScalaUnidocPlugin, ScalaJSPlugin)

lazy val coslideJVM =
  coslide
    .jvm
    .settings(name := "coslideJVM")

lazy val coslideJS =
  coslide
    .js
 //   .enablePlugins(ScalaJSBundlerPlugin)
    .settings(
      name := "coslideJS",
      scalaJSUseMainModuleInitializer := true,
//      npmDependencies in Compile += "highlight.js" -> "9.12.0",
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.9.1"
    //    "com.github.karasiq" %%% "scalajs-marked" % "1.0.2"
      )
    )

lazy val root = (project in file("."))
  .aggregate(coslideJVM, coslideJS)
  .settings(
    publish := {},
    publishLocal := {}
  )
