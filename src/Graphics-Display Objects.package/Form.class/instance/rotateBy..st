scaling, rotation
rotateBy: deg
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."

	^ self rotateBy: deg smoothing: 1
"
 | a f |  f := Form fromDisplay: (0@0 extent: 200@200).  a := 0.
[Sensor anyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor cursorPoint extent: 130@66))
		rotateBy: (a := a+5)) display].
f display
"