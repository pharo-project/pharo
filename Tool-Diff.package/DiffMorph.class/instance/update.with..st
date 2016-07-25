updating
update: aSymbol with: aValue

	aSymbol == #showOnlyDestination
		ifTrue: [ ^ self showOnlyDestination: aValue ].
	aSymbol == #showOptions	
		ifTrue: [ ^ self showOptions: aValue ].
		
	^ super update: aSymbol with: aValue