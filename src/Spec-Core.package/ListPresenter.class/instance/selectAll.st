api
selectAll
	self listItems withIndexDo: [ :each :i | self setSelectionStateFor: i at: true ].
	self changed: #listElementAt: