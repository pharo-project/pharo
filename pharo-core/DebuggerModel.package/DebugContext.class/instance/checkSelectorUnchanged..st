private
checkSelectorUnchanged: aSelector
	| unchanged |
	unchanged := aSelector == self selectedMessageName
		or: [ self selectedMessageName isDoIt and: [ aSelector numArgs = self selectedMessageName numArgs ] ].
	unchanged
		ifFalse: [ self inform: 'can''t change selector' ].
	^ unchanged