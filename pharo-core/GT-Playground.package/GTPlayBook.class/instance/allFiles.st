accessing-dynamic
allFiles
	^ self class cacheDirectory files select: [ :each | 
		each extension = GTPlayPage fileExtension ]