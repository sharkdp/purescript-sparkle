// module Test.FlareCheck
// jshint browser: true
// jshint node: true

"use strict";

exports.appendTest = function(parentId) {
  return function(title) {
    return function(elements) {
      return function() {
        var parent = document.getElementById(parentId);
        var fieldset = document.createElement("fieldset");
        fieldset.className = "flarecheck-test";
        var legend = document.createElement("legend");
        legend.textContent = title;
        fieldset.appendChild(legend);

        for (var i = 0; i < elements.length; i++) {
          fieldset.appendChild(elements[i]);
        }

        var output = document.createElement("div");
        output.className = "flarecheck-output";
        fieldset.appendChild(output);

        parent.appendChild(fieldset);

        return output;
      };
    };
  };
};

// From http://stackoverflow.com/a/6234804/704831
exports.escapeHTML = function(unsafe) {
  return unsafe
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;");
};

exports.setText = function(output) {
  return function(str) {
    return function() {
      var pre = output.firstChild;
      if (!pre) {
        pre = document.createElement("pre");
        pre.className = "flarecheck-output-settext";
        output.appendChild(pre);
      }
      pre.textContent = str;
    };
  };
};

exports.setHTML = function(output) {
  return function(code) {
    return function() {
      output.innerHTML = code;
    };
  };
};

// vim: ts=2:sw=2
