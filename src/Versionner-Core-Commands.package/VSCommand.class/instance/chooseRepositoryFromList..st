repositories
chooseRepositoryFromList: repositories
	repositories size > 1
		ifTrue: [ 
			^ UIManager default
				chooseFrom: (repositories collect: [ :repository | repository description ])
				values: repositories
				title: 'Select repository' ].
	^ repositories first