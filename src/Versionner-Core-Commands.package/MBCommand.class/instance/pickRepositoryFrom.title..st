repositories
pickRepositoryFrom: repositories title: title
	| index |
	index := self uiManager chooseFrom: (repositories collect: [ :ea | ea description ]) title: title.
	^ index = 0
		ifFalse: [ repositories at: index ] 