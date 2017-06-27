*CodeExport
printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod 
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream cr; nextPut: $!.
	aFileStream nextChunkPut: (String streamContents:
		[:strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		(changeStamp ~~ nil and:
			[changeStamp size > 0 or: [priorMethod ~~ nil]]) ifTrue:
			[strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod ~~ nil ifTrue:
			[strm nextPutAll: ' prior: '; print: priorMethod sourcePointer]]).
	