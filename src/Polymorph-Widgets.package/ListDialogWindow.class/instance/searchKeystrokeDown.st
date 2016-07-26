events
searchKeystrokeDown
	|interval|
	interval := searchMorph selectionInterval .
	((interval last == searchMorph content size)
	and: [ interval last < interval first ])
		ifFalse: [ ^ false ].
		
	self giveFocusToList.
	^ true