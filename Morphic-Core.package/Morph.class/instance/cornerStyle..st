rounding
cornerStyle: aSymbol
	"This method makes it possible to set up desired corner style. aSymbol has to be one of:
		#square
		#rounded"

	aSymbol == self cornerStyle ifFalse:[
		self assureExtension.
		extension cornerStyle: aSymbol.
		self changed]