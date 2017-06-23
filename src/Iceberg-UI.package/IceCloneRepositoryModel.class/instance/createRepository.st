actions
createRepository
	[
		| repository |
		
		repository := IceRepositoryCreator new 
			remote: (IceRemote url: remoteUrl text trim);
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