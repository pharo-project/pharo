accessing-path
file
	^ self isDirectoryPath
		ifTrue: [ String new ]
		ifFalse: [ segments last ]