testing
isPossibleBaseline

	self 
		projectDo: [:prj | prj isPossibleBaseline ifFalse: [ ^false ]] 
		packageDo: [:pkg | pkg isPackageLoaded ifFalse: [ ^false ]] 
		groupDo: [:ignored | ].
	^true