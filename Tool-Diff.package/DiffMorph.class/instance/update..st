updating
update: aSymbol

	aSymbol == #addOptions
		ifTrue: [ ^ self addOptionsPanel ].
	aSymbol == #removeOptions	
		ifTrue: [ ^ self removeOptionsPanel ].
		
	^ super update: aSymbol