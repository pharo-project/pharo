callbacks
selection: aSelection 
	self selection = aSelection ifTrue: [ ^ self ].
	super selection: aSelection.
	self announcer announce: (
			GLMTreeMorphSelectionChanged new 
				selectionValue: (aSelection ifNotNil: [aSelection selectedItemOrItemsOrNil]);
				selectionPathValue: self selectionPathItems;
				yourself)