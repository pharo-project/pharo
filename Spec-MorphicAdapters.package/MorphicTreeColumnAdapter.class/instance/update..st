updating
update: aSymbol

	aSymbol == #displayBlockChanged ifTrue: [ ^ self displayBlockChanged ].
	aSymbol == #resizableChanged ifTrue: [ ^ self resizableChanged ].
	aSymbol == #headerLabelChanged ifTrue: [ ^ self headerLabelChanged ].
	aSymbol == #headerFontChanged ifTrue: [ ^ self headerFontChanged ].
	aSymbol == #headerIconChanged  ifTrue: [ ^ self headerIconChanged ].
	
	^ super update: aSymbol