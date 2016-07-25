morphic ui
repositoryMenu: aMenu
	^self fillMenu: aMenu fromSpecs: #(
		('Add repository...' addRepository)
	)