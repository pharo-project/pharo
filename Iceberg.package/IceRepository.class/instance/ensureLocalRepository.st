repository handling
ensureLocalRepository
	repository ifNil: [ 
		self searchExistingRepository
			ifNotNil: [ :repo | self repository: repo ]
			ifNil: [ self createRepository ]
	]