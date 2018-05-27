"use strict";

var Main = require("../output/App.Main");

Main.main();

if (module.hot) {
  module.hot.accept(function() {
    // Delete everything of previous Halogen app
    document.getElementById("app").innerHTML = "";
    // Rerun main again
    Main.main();
  });
}
