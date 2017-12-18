'use strict';

exports.supportsGetUserMedia = function(navigator) {
  return navigator.mediaDevices && navigator.mediaDevices.getUserMedia;
};
