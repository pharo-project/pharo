accessing
converter

	converter ifNil: [self converter: TextConverter defaultSystemConverter].
	^ converter
