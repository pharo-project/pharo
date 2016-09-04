menu
defineAllRepositoriesInMCBrowserButton: aList

	<menu>
	aList
		act: [ :presenter | self defineAllRepositoriesInMCBrowser ]
		icon: (self iconNamed: #monticelloPackageIcon)
		entitled: 'Define all distribution catalog MC repositories'