resolving
resolveString: aString
	| decoded fs |
	decoded := FilePathEncoder decode: aString.
	fs := FileSystem disk.
	^ FileReference 
		fileSystem: fs 
		path: (fs pathFromString: decoded)