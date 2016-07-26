scaling, rotation
magnify: aRectangle by: scale smoothing: cellSize
	"Answer a Form created as a scaling of the receiver.
        Scale may be a Float or even a Point, and may be greater or less than 1.0."

	| newForm |
	newForm := self blankCopyOf: aRectangle scaledBy: scale.
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		cellSize: cellSize;
		"installs a new colormap if cellSize > 1"
			combinationRule: 3;
		copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
	^ newForm

	"Dynamic test...
[Sensor anyButtonPressed] whileFalse:
        [(Display magnify: (Sensor cursorPoint extent: 131@81) by: 0.5 smoothing: 2) display]
"
	"Scaling test...
| f cp | f := Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor anyButtonPressed] whileFalse:
        [cp := Sensor cursorPoint.
        (f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent smoothing: 2) display]]
"