actions
createRepository
	[
		| repository |
		
	self projectName isEmptyOrNil 
		ifTrue: [ Error signal: 'Project name is required' ].
		repository := IceRepositoryCreator new 
			location: self location / self projectName;
			subdirectory: subdirectory text;
			createNewRepositoryNamed: self projectName.
		repository register.
		self window delete.
		acceptBlock ifNotNil: [ acceptBlock cull: self ] ]
	on: IceError 
	do: [ :error | 
		UIManager default 
			abort: error messageText 
			title: 'Could not create repository' ].
