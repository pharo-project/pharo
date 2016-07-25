private
implementorsDescription: aSymbol 
	| implementors output |
	output := WriteStream on: String new.
	implementors := self systemNavigation allImplementorsOf: aSymbol.
	implementors isEmpty ifTrue: 
		[ ^ NECEntryDescription 
			label: 'symbol'
			title: '(no implementors)'
			description: 'This is just symbol.' ].
	implementors do: [ :each | 
		output
			nextPutAll: each className printString;
			cr ].
	^ NECEntryDescription 
		label: self label
		title: '(Implementors)'
		description: output contents