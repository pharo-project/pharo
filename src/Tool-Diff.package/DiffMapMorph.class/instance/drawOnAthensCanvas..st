drawing
drawOnAthensCanvas: anAthensCanvas
	"Draw the indicators for the mappings."
	
	|b f|
	b := self innerBounds insetBy: 2.
	super drawOnAthensCanvas: anAthensCanvas.
	b height < 1 ifTrue: [^self].
	f := self mappingsHeight.
	f < 1 ifTrue: [^self].
	f := b height / f.
	anAthensCanvas asCanvasWrapper clipBy: self clippingBounds during: [:c |
		self mappings do: [:j |
			j drawMapOn: c in: b scale: f]]