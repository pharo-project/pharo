Extension { #name : #StringMorph }

{ #category : #'*FreeType-Morphic' }
StringMorph >> changeFont [
	| newFont chooser |
	chooser := FontChooser 
		windowTitle: 'Choose a Font' 
		for: self 
		setSelector: #font: 
		getSelector: self fontToUse.
	chooser openModal.
	newFont := chooser result.
	newFont ifNotNil: [self font: newFont].
]
