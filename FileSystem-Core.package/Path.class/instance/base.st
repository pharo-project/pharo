accessing
base
	"Returns the base of the basename, i.e. /foo/gloops.taz basename is 'gloops'"
	^ self basename copyUpTo: self extensionDelimiter