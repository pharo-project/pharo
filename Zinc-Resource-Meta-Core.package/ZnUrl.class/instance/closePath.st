accessing-path
closePath
	self isDirectoryPath
		ifFalse: [ self addPathSegment: #/ ]