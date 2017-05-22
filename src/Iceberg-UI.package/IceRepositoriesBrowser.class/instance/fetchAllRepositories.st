actions
fetchAllRepositories
	UIManager default informUserDuring: [ :bar |
		| repositoriesCount |
		repositoriesCount := IceRepository registry size.
		bar max: repositoriesCount + 1.
		
		IceRepository registry doWithIndex: [ :repository :index|
			bar label: 
				('Looking for incoming commigs in all repositories ({1} of {2})'
					format: {index. repositoriesCount}).
			bar current: index.
			
			repository fetch.
		]]
	 