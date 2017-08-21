accessing
extension
	"Return the extension of path basename i.e., /foo/gloops.taz extension is 'taz'"
	
	^ self basename copyAfterLast: self extensionDelimiter