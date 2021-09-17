load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test")
package(default_visibility = ["//visibility:public"])

scala_library(
    name = "bazel",
    srcs = ["Bazel.scala"],
    deps = ["//src:parser"],
    exports = ["//src:parser"])

scala_test(
    name = "spec",
    srcs = ["BazelSpec.scala"],
    deps = [":bazel", "//src:io"])

scala_binary(
    name = "main",
    srcs = ["Main.scala"],
    main_class = "bentkus.parser.bazel.Main",
    deps = [":bazel", "//src:io"])
    #jvm_flags=["-Xrunhprof:cpu=samples,file=myprogram.hprof"])
