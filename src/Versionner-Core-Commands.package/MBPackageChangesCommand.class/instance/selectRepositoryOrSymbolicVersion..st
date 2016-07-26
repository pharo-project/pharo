utility
selectRepositoryOrSymbolicVersion: workingCopy
	| project symbolicVersions repos list values |
	project := target spec project.
	symbolicVersions := project symbolicVersionSymbols select: [ :each | project hasVersion: each ].
	repos := workingCopy repositoryGroup repositories.
	list := (symbolicVersions collect: [:each | each printString]), (repos collect: [ :r | r description ]).
	values := symbolicVersions, repos.
	^self uiManager
		chooseFrom: list
		values: values
		lines: {symbolicVersions  size}
		title: 'Please choose which symbolic version or repository'.
