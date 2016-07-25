simulating
clickOnListItem: aString
	| listMorph |
	listMorph := self findListContaining: aString.
	listMorph changeModelSelection: (listMorph getList indexOf: aString).