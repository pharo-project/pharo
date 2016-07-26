events
searchKeystrokeUp
	(searchMorph selectionInterval last == 0)
		ifFalse: [ ^ false ].
		
	self listIndex: list size.
	self giveFocusToList.
	^ true