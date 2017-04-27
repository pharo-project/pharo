accessing
elements: aCollection

	elements := aCollection collect: [ :each | each -> 0 ].
	
	matchedElements := elements.
	
	dataSource elements: matchedElements.
	
	self update
	