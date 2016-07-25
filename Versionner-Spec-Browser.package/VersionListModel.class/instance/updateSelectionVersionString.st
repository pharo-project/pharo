updating
updateSelectionVersionString
	| selectedItem |
	
	selectedItem := selectionHolder selection value.
	selectionVersionString := selectedItem 
									ifNil: [ nil ]
									ifNotNil: [ selectedItem versionString ].
	selectionVersionString ifNil: [ ^self ]