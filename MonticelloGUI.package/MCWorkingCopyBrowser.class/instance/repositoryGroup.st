morphic ui
repositoryGroup
	"Return the group of repository displayed in the monticello browser"
	(showOnlyRepositoriesFromWorkingCopy and: [ workingCopy isNotNil ])
		ifTrue: [ ^ workingCopy repositoryGroup ].
	^ MCRepositoryGroup default