operations
replaceWith: aString

	self cut.
	self end insert: aString.
	start moveTo: end