drawing
drawOn: aCanvas
	self useSelectionBar
		ifTrue: [self paragraph displaySelectionBarOn: aCanvas].
	self useExtraSelection 
		ifTrue: [self paragraph displayExtraSelectionOn: aCanvas].
	super drawOn: aCanvas.
