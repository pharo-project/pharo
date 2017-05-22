writing
addString: string at: fileNameOrPath encodedTo: ignored
    "fileNameOrPath may have one or two elements"
    "encodeTo: arg, because FileTree uses UTF8 by default"

	| split path fileName |
	split := fileNameOrPath lastIndexOf: $/.
	path := fileNameOrPath first: split.
	fileName := fileNameOrPath copyFrom: split + 1.
	
	(self packageFileDirectory entryByPath: path)
		addEntryNamed: fileName withContents: string.
