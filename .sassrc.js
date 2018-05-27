"use strict";
/* eslint-env node */

var path = require("path");
var CWD = process.cwd();

module.exports = {
  "includePaths": [
    path.resolve(CWD, "bower_components"),
    path.resolve(CWD, "frontend/styles"),
  ]
};
