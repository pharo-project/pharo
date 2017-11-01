Extension { #name : #StringMorph }

{ #category : #'*FreeType-override' }
StringMorph >> changeFont [
	| newFont chooser |
	chooser := FontChooser 
		windowTitle: 'Choose a Font' 
		for: self 
		setSelector: #font: 
		getSelector: self fontToUse.
	self openModal: chooser.
	newFont := chooser result.
	newFont ifNotNil: [self font: newFont].
]
