tests
testUpdatePresentationTitle
	browser panes first presentations first title: 'something'.
	browser panes first presentations first update.
	self assert: listLabelMorph contents equals: 'something'