filein/out
saveContents: fullFileName
	"Save the contents of a gzipped file"
	| zipped buffer unzipped newName |
	newName := fullFileName copyUpToLast: FileSystem disk extensionDelimiter.
	unzipped := FileStream newFileNamed: newName.
	unzipped binary.
	zipped := self on: (FileStream readOnlyFileNamed: fullFileName).
	buffer := ByteArray new: 50000.
	'Extracting ' , fullFileName
		displayProgressFrom: 0
		to: zipped sourceStream size
		during: 
			[:bar | 
			[zipped atEnd]
				whileFalse: 
					[bar current: zipped sourceStream position.
					unzipped nextPutAll: (zipped nextInto: buffer)].
			zipped close.
			unzipped close].
	^ newName