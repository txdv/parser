load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_library", "scala_test")
package(default_visibility = ["//visibility:public"])

scala_library(
    name = "parser",
    srcs = ["Parser.scala", "package.scala"])

scala_test(
    name = "parser_spec",
    srcs = ["ParserSpec.scala"],
    deps = [":parser"])

scala_binary(
    name = "parser_main",
    main_class = "bentkus.parser.Main",
    srcs = ["ParserMain.scala"],
    deps = [":parser"])

scala_library(
    name = "json",
    srcs = ["Json.scala"],
    deps = [":parser"],
    exports = [":parser"])

scala_test(
    name = "json_spec",
    srcs = ["JsonSpec.scala"],
    deps = [":json"])

scala_library(
    name = "html",
    srcs = ["Html.scala"],
    deps = [":parser"],
    exports = [":parser"])

scala_test(
    name = "html_spec",
    srcs = ["HtmlSpec.scala"],
    deps = [":html"])

scala_binary(
    name = "html_test",
    main_class = "bentkus.parser.HtmlTest",
    srcs = ["HtmlTest.scala"],
    deps = [":html", ":css"])

scala_library(
    name = "css",
    srcs = ["Css.scala"],
    deps = [":parser"],
    exports = [":parser"])

scala_test(
    name = "css_spec",
    srcs = ["CssSpec.scala"],
    deps = [":css"])

scala_binary(
    name = "test",
    main_class = "bentkus.parser.Test",
    srcs = ["Test.scala"])

scala_library(
    name = "io",
    srcs = ["IO.scala"])
