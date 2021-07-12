load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test")
package(default_visibility = ["//visibility:public"])

scala_library(
    name = "parser",
    srcs = ["Parser.scala", "package.scala"])

scala_test(
    name = "parser_spec",
    srcs = ["ParserSpec.scala"],
    deps = [":parser"])
