# Package

version       = "0.1.0"
author        = "Double-oxygeN"
description   = "XFRP compiler"
license       = "BSD-3-Clause"
srcDir        = "src"
bin           = @["xfrp"]


# Dependencies

requires "nim >= 1.4.8"
requires "nimly >= 0.7"
requires "patty >= 0.3.4"

task docgen, "Generate documentations":
  switch("project")
  switch("outDir", "docs")
  switch("git.url", "https://github.com/psg-titech/xfrp-nim")
  switch("git.commit", "devel")
  setCommand "doc", "src/xfrp"
