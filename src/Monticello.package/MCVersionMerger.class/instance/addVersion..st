adding
addVersion: aVersion
	
	records add: (MCMergeRecord version: aVersion).
	aVersion dependencies 
		do: [:ea | | dep |
			dep := ea resolve.
			(records anySatisfy: [:r | r version = dep]) 
				ifFalse: [self addVersion: dep]]
		displayingProgress: [ :ea| 'Searching dependency: ', ea package name]