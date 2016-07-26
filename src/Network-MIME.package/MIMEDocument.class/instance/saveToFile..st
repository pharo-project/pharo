files
saveToFile: anAbsolutePathString

	FileStream forceNewFileNamed: anAbsolutePathString do: [ :str |
		str binary.
		str nextPutAll: (self contents) ].