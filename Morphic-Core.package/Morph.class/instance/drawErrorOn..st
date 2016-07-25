drawing
drawErrorOn: aCanvas
	"The morph (or one of its submorphs) had an error in its drawing method."
	aCanvas
		frameAndFillRectangle: bounds
		fillColor: Color red
		borderWidth: 1
		borderColor: Color yellow.
	aCanvas line: bounds topLeft to: bounds bottomRight width: 1 color: Color yellow.
	aCanvas line: bounds topRight to: bounds bottomLeft width: 1 color: Color yellow.
	
	self valueOfProperty: #drawError ifPresentDo: [ :error| | trace stringBounds |
		trace := String streamContents: [ :s| error signalerContext shortDebugStackOn: s].
		stringBounds := bounds insetBy: 5.
		trace linesDo: [ :aString|
			aCanvas drawString: aString in: stringBounds.
			stringBounds := stringBounds top: stringBounds top + (TextStyle defaultFont pixelSize * 1.2) ]]