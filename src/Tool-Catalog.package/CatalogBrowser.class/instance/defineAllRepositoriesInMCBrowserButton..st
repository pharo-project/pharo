menu
defineAllRepositoriesInMCBrowserButton: aList

	<menu>
	aList
		act: [ :presenter | self defineAllRepositoriesInMCBrowser ]
		icon: (Smalltalk ui icons iconNamed: #monticelloPackageIcon)
		entitled: 'Define all distribution catalog MC repositories'