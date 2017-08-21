resolving
resolveString: aString
	| decoded fs |
	"The argument string is actually a byte array encoded differently on each platform.
	We are transforming it to an image string.
	We assume for now that the string is utf8 encoded."
	decoded := aString asByteArray utf8Decoded.
	fs := FileSystem disk.
	^ FileReference 
		fileSystem: fs 
		path: (fs pathFromString: decoded)