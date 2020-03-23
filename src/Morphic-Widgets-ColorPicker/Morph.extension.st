Extension { #name : #Morph }

{ #category : #'*Morphic-Widgets-ColorPicker' }
Morph >> changeColor [
	
	| dialog |
	dialog := ColorSelectorDialogWindow new
		title: 'Choose color';
		selectedColor: self color.
	dialog openModal.
	dialog cancelled
		ifFalse: [self fillStyle: dialog selectedColor]
	
	
]

{ #category : #'*Morphic-Widgets-ColorPicker' }
Morph >> scaledIntoFormOfSize: aSmallInteger [ 
	^ self imageForm scaledIntoFormOfSize: aSmallInteger 
]
