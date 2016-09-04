menu
defineCurrentVersionRepositoryButton: aList

	<menu>
	aList
		act: [ :presenter | self defineCurrentVersionRepositoryInMCBrowser ]
		icon: (self iconNamed: #komitterIcon)
		entitled: 'Define current distribution catalog MC repository'