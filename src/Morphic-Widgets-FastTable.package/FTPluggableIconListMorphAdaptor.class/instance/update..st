updating
update: symbol
	(symbol == self getListSelector or: [ symbol == self getListElementSelector ]) 
		ifTrue: [ ^ self updateList ].
		
	symbol == self getIndexSelector
		ifTrue: [ ^ self updateSelectionIndex ].