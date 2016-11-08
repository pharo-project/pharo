menu
defineAllRepositoriesInMCBrowserButton: aList

	<menuOrder: 9>
	aList
		act: [ :presenter | self defineAllRepositoriesInMCBrowser ]
		iconName: #monticelloPackageIcon
		entitled: 'Define all distribution catalog MC repositories'