filein/out
writeUncompressedOn: file
	"Write the receiver on the file in the format depth, extent, offset, bits.  Warning:  Caller must put header info on file!  Use writeUncompressedOnFileNamed: instead."
	self unhibernate.
	file binary.
	file nextPut: depth.
	file nextWordPut: width.
	file nextWordPut: height.
	file nextWordPut: ((self offset x) >=0
					ifTrue: [self offset x]
					ifFalse: [self offset x + 65536]).
	file nextWordPut: ((self offset y) >=0
					ifTrue: [self offset y]
					ifFalse: [self offset y + 65536]).
	bits writeUncompressedOn: file