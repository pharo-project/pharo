visitor-double dispatching
visitParseErrorNode: anErrorNode  

	super visitParseErrorNode: anErrorNode.
	methodBuilder popTop.
