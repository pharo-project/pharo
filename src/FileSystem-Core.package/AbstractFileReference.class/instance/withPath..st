navigating
withPath: newPath
	^ self path == newPath
		ifTrue: [ self ]
		ifFalse: [ self copyWithPath: newPath ]