testing
isDirectoryPath
	^ self hasPath 
		ifTrue: [ segments last = #/ ]
		ifFalse: [ true ]
	