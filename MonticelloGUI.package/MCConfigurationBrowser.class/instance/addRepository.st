repositories
addRepository
	(self pickRepositorySatisfying: [:ea | (self repositories includes: ea) not])
		ifNotNil: [:repo |
			(repo isKindOf: MCHttpRepository)
				ifFalse: [^self inform: 'Only HTTP repositories are supported'].
			self repositories add: repo.
			self repositoryListChanged.
		]