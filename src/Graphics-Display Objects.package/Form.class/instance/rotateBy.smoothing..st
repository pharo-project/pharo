scaling, rotation
rotateBy: deg smoothing: cellSize
	"Rotate the receiver by the indicated number of degrees."
	^self rotateBy: deg magnify: 1 smoothing: cellSize
"
 | a f |  f := Form fromDisplay: (0@0 extent: 200@200).  a := 0.
[Sensor anyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor cursorPoint extent: 130@66))
		rotateBy: (a := a+5) smoothing: 2) display].
f display
"