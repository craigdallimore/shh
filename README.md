purescript-shh
==============

A play app for trying out purescript-halogen and PWA practices.

Todo
----

- [ ] Add LICENCE
- [/] env : show browser warning if feature not available
- [ ] button click -> prompt for microphone
- [ ] with microphone access, show record button
- [ ] record to blob
- [ ] record length should be constrained / show error if too long
- [ ] 'play audio' button
- [ ] 'stop' button
- [ ] PWA stuff - how can we make this installable / good on android?
 - [ ] web app manifest
 - [ ] icons

UX
----

If we fail to have the required browser features, we should show a "please use another browser"
- [ ] style that empty state
- [ ] nojs
Given the required features are available, we see
- [ ] a record button
- [ ] an empty list of past recordings

Clicking the record button will
- [ ] prompt for the ability to use the microphone (if it is not already given)
- [ ] start recording
- [ ] change the record button to a "stop" button
- [ ] present a recording waveform

Clicking the stop button should
- [ ] add a new recording to the list
- [ ] hide the stop button
- [ ] show the record button

list items should include
- [ ] the ability to name a recording
- [ ] change the start and end
- [ ] remove the recording
- [ ] present the waveform
- [ ] play once
- [ ] play on loop
