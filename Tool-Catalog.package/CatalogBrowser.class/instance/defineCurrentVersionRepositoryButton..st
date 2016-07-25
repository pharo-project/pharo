menu
defineCurrentVersionRepositoryButton: aList

	<menu>
	aList
		act: [ :presenter | self defineCurrentVersionRepositoryInMCBrowser ]
		icon: (Smalltalk ui icons iconNamed: #komitterIcon)
		entitled: 'Define current distribution catalog MC repository'