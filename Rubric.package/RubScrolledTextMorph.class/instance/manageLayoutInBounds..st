layout
manageLayoutInBounds: aRectangle
	| myBounds |
	myBounds := aRectangle.
	self horizontalRulers do: [ :r | myBounds := r manageLayoutInBounds: myBounds ].
	self verticalRulers do: [ :r | myBounds := r manageLayoutInBounds: myBounds ].
	self overRulers do: [ :r | myBounds := r manageLayoutInBounds: myBounds ].
	self scrollPane
		ifNotNil: [ :scrollpane |
			scrollpane position: myBounds topLeft.
			scrollpane extent: (myBounds extent max: 0@0)].
	^ myBounds