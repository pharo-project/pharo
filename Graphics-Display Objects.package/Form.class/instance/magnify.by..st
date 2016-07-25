scaling, rotation
magnify: aRectangle by: scale 
	"Answer a Form created as a scaling of the receiver.
	Scale may be a Float, and may be greater or less than 1.0."
	^ self magnify: aRectangle by: scale smoothing: 1

"Dynamic test...
[Sensor anyButtonPressed] whileFalse:
	[(Display magnify: (Sensor cursorPoint extent: 31@41) by: 5@3) display]
"
"Scaling test...
| f cp | f := Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor anyButtonPressed] whileFalse:
	[cp := Sensor cursorPoint.
	(f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent) display]]
"
"Consistency test...
 | f f2 p | [Sensor anyButtonPressed] whileFalse:
	[f := Form fromDisplay: ((p := Sensor cursorPoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 := f magnify: f boundingBox by: 5@3.
	(f2 shrink: f2 boundingBox by: 5@3) displayAt: p]
"
