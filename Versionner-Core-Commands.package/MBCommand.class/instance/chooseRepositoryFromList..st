utility
chooseRepositoryFromList: repos
	"Return the repository of the configuration. Create one (using UI) if necessary"

	repos size > 1
		ifTrue: [ 
			^ self uiManager
				chooseFrom: (repos collect: [ :repo | repo description ])
				values: repos
				title: 'Select repository' ].
	^ repos first