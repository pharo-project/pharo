updating
update: aSymbol

	aSymbol == stateSelector
		ifTrue: [ ^ self updateState ].
	aSymbol == onImageSelector
		ifTrue: [ ^ self updateOnImage ].
	aSymbol == offImageSelector
		ifTrue: [ ^ self updateOffImage ].
	aSymbol == pressedImageSelector
		ifTrue: [ ^ self updatePressedImage ].
				
	^ super update: aSymbol