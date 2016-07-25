updating
update: aSymbol

	aSymbol = #select
		ifTrue: [ ^ self selected: true ].
		
	aSymbol = #deselect
		ifTrue: [ ^ self selected: false ].
		
	aSymbol = #takeHighlight
		ifTrue: [ ^ self takeHighlight ].

	super update: aSymbol