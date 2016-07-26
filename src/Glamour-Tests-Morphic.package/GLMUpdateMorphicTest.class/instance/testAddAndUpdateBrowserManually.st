tests
testAddAndUpdateBrowserManually
	entity add: 4.
	self assert: unupdatedListMorph model roots size equals: 3.
	browser update.
	self assert: unupdatedListMorph model roots size equals: 4.
	entity add: 5.
	browser panes first presentations update.
	self assert: unupdatedListMorph model roots size equals: 5