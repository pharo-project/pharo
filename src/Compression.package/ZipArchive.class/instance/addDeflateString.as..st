archive operations
addDeflateString: aString as: aFileName
	"Add a verbatim string under the given file name"
	| mbr |
	mbr := self addString: aString as: aFileName.
	mbr desiredCompressionMethod: CompressionDeflated.
	^mbr