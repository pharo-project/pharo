printing
configMethodOn: aStream indent: indent

	aStream nextPutAll: self class name asString, ' member: ('.
	self spec configMethodOn: aStream indent: indent.
	aStream nextPutAll: ')'.