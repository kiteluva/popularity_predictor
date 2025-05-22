// www/script.js (Minimal Content)

$(document).ready(function() {
  console.log("Minimal script.js: jQuery is ready!");
  // Check if Shiny is defined
  if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
    Shiny.setInputValue("js_loaded_message", true, { priority: "event" });
    console.log("Minimal script.js: Sent 'js_loaded_message' to Shiny");
  } else {
    console.warn("Minimal script.js: Shiny is not defined.");
  }
})
