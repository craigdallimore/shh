"use strict";

exports._getUserMedia = function(onError, onSuccess) {
  navigator.mediaDevices.getUserMedia({ audio: true })
    .then(onSuccess)
    .catch(onError);

  return function(cancelError, cancellerError, cancellerSuccess) {
    cancellerSuccess();
  };
};
