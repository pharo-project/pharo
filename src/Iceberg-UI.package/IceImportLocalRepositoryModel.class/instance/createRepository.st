actions
createRepository
	self location ifNil: [ 
		UIManager default 
			abort: 'Please select a repository location'
			title: 'Could not create repository'. 
		^ self ].

	[
		| repository |
		
		repository := IceRepositoryCreator new 
			location: self location;
			subdirectory: subdirectory text;
			createRepository.
		repository register.
			
		self window delete. 
	]
	on: IceError 
	do: [ :error | 
		UIManager default 
			abort: error messageText 
			title: 'Could not create repository' ]