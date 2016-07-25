as yet unclassified
drawOn: aCanvas
	"Draw the join on the given canvas."
	
	super drawOn: aCanvas.
	self stateIcon ifNotNil: [:i |
		aCanvas
			translucentImage: i
			at: self stateIconBounds topLeft]