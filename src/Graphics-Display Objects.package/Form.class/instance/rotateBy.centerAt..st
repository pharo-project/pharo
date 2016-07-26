scaling, rotation
rotateBy: direction centerAt: aPoint
	"Return a rotated copy of the receiver. 
	direction = #none, #right, #left, or #pi"
	| newForm quad rot scale |
	direction == #none ifTrue: [^ self].
	scale :=  (direction = #pi ifTrue: [width@height] ifFalse: [height@width]) / self extent .
	newForm := self blankCopyOf: self boundingBox scaledBy: scale.
	quad := self boundingBox innerCorners.
	rot := #(right pi left) indexOf: direction.
	(WarpBlt current toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: ((1+rot to: 4+rot) collect: [:i | quad atWrap: i])
			 toRect: newForm boundingBox.
	newForm offset: (self offset rotateBy: direction centerAt: aPoint).
	^ newForm
"
[Sensor anyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor cursorPoint extent: 130@66))
		rotateBy: #left centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p | [Sensor anyButtonPressed] whileFalse:
	[f := Form fromDisplay: ((p := Sensor cursorPoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 := f rotateBy: #left centerAt: 0@0.
	(f2 rotateBy: #right centerAt: 0@0) displayAt: p]
"
