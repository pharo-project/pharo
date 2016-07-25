accessing
basenameWithoutExtension
	"Returns the base of the basename but without its extension, 
		i.e. 
		/foo/gloops.taz basenameWithoutExtension is 'gloops'
		/ basenameWithoutExtension is '/'"
	
	^ self basename copyUpToLast: self extensionDelimiter