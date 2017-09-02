// jshint browser: true
// jshint node: true

"use strict";

exports.appendTest = function(parentId) {
  return function(title) {
    return function(doc) {
      return function(elements) {
        return function() {
          var parent = document.getElementById(parentId);
          var fieldset = document.createElement("fieldset");
          fieldset.className = "sparkle-test";
          var legend = document.createElement("legend");
          legend.textContent = title;
          fieldset.appendChild(legend);

          if (doc !== "") {
            var docEl = document.createElement("p");
            docEl.innerHTML = doc;
            fieldset.appendChild(docEl);
          }

          for (var i = 0; i < elements.length; i++) {
            fieldset.appendChild(elements[i]);
          }

          var output = document.createElement("div");
          output.className = "sparkle-output";
          fieldset.appendChild(output);

          parent.appendChild(fieldset);

          return output;
        };
      };
    };
  };
};

exports.setText = function(output) {
  return function(str) {
    return function() {
      var pre = output.firstChild;
      if (!pre) {
        pre = document.createElement("pre");
        pre.className = "sparkle-output-settext";
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
