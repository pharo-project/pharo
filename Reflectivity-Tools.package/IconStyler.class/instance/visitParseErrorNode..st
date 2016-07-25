visiting
visitParseErrorNode: anErrorNode 
	self addIconStyle: anErrorNode.
	super visitParseErrorNode: anErrorNode.