accessing
changeFilterRequestFor: aGTInspector

	self
		openListChooseLabeled: 'All available tags' 
		forInspector: aGTInspector
		with: allTags 
		select: selectedTags 
		format: #printString
		actionSelector: #replaceSelectedTagsWith:in:
