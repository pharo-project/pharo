initialize-release
writeRepresentation
	representation := self write contents.
	self sizeBuffer: representation size + 1024 