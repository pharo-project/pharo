accessing
manageLayoutInBounds: aRectangle
	| ret |
	self handleTextChangeWhile: [ ret := super manageLayoutInBounds: aRectangle ].
	^ ret