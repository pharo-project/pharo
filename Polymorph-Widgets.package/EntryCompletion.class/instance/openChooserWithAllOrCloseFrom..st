accessing
openChooserWithAllOrCloseFrom: aFieldMorph 
	chooser 
		ifNotNil: [self closeChooser]
		ifNil: [self openChooserWith: nil from: aFieldMorph].
	^ chooser