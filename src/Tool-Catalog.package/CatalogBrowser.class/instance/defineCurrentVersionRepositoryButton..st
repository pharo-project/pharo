menu
defineCurrentVersionRepositoryButton: aList

	<menuOrder: 7>
	aList
		act: [ :presenter | self defineCurrentVersionRepositoryInMCBrowser ]
		iconName: #komitterIcon
		entitled: 'Define current distribution catalog MC repository'