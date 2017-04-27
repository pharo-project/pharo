menu - operations
browseSelection

	| selectionSize |
	selectionSize := self selectedEntryReferences size.	
	selectionSize > 3 ifTrue: [
		(self confirm: 'Do you want to open a browser for each code changes? (', selectionSize asString, ' items selected)')
			ifFalse: [ ^self ] ].

	self selectedEntryItems 
		select: [ :each | each entry content isCodeChange ] 
		thenDo: [ :each | each browseItem ]