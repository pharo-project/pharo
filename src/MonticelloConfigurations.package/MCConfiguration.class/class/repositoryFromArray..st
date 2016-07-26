converting
repositoryFromArray: anArray
	^ MCRepositoryGroup default repositories
		detect: [:repo | repo description = anArray first]
		ifNone: [
			MCHttpRepository
				location: anArray first
				user: ''
				password: '']