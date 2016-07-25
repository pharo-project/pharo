accessing
text: aString

	self privateSetText: aString.
	self wrappedBounds markWidthDirty.