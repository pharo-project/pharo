accessing
packageNameWithStar
	^ modified
		ifTrue: ['* ', self packageName]
		ifFalse: [self packageName]