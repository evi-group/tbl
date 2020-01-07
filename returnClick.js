$(document).keyup(function(event) {
  if ($("#passwd").is(":focus") && (event.keyCode == 13)) {
    $("#login").click();
  }
});