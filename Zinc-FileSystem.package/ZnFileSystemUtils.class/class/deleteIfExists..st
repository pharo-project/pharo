paths
deleteIfExists: path
	^ (self exists: path)
		ifTrue: [ self delete: path. true ]
		ifFalse: [ false ]