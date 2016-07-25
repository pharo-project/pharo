testing
hasVersion: aVersionString
 	[ self project version: aVersionString ]
		on: MetacelloSymbolicVersionDoesNotExistError
		do:  [ ^ false ].
	^ true