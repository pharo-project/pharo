accessing
borderWidth: newWidth
	super borderWidth: newWidth.
	paragraph ifNotNil: [self composeToBounds].