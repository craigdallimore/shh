"use strict";

exports.getUserMedia = function(onSuccess) {
  return function(onError) {
    return navigator.getUserMedia(
      { audio: true },
      function(s) { onSuccess(s)() },
      function(e) { onError(e)() }
    );
  };
};
