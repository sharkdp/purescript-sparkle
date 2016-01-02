// module Test.FlareCheck

exports.appendTest = function(parentId) {
  return function(title) {
    return function(elements) {
      return function() {
        var parent = document.getElementById(parentId);
        var fieldset = document.createElement("fieldset");
        fieldset.className = "flarecheck-test";
        var legend = document.createElement("legend");
        legend.innerText = title;
        fieldset.appendChild(legend);

        for (var i = 0; i < elements.length; i++) {
          fieldset.appendChild(elements[i]);
        }

        preOutput = document.createElement("pre");
        preOutput.className = "flarecheck-output";
        fieldset.appendChild(preOutput);

        parent.appendChild(fieldset);

        return preOutput;
      };
    };
  };
};

exports.printOutput = function(el) {
  return function(output) {
    return function() {
      el.innerHTML = output;
    };
  };
};

// vim: ts=2:sw=2
