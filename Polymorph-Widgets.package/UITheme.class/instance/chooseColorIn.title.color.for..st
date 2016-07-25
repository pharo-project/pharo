services
chooseColorIn: aThemedMorph title: aString color: aColor for: aBlock
	"execute block with result of a color selector dialog with the given title and initial color."

	|d newColor|
	d := (ColorSelectorDialogWindow newWithTheme: aThemedMorph theme)
			title: aString;
			selectedColor: aColor.
	d extent: d initialExtent.
	d announcer when: ColorChanged do: [:ann | 
		aBlock value: ann newColor].
	d openInHand.
	self runModal: d.
	
	newColor := d  cancelled
		ifTrue: [aColor]
		ifFalse: [d selectedColor].
				
	aBlock value: newColor.