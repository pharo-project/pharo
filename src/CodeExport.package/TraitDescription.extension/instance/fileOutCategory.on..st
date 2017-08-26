*CodeExport
fileOutCategory: aSymbol on: aFileStream
	"File a description of the receiver's category, aString, onto aFileStream. If 
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.sources file, and should only write one preamble per method category."

	| selectors |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aSymbol.

	"Overridden to preserve author stamps in sources file regardless"
	selectors do: [:sel |
		self printMethodChunk: sel on: aFileStream ].
	^ self