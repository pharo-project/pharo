displaying
displayErrorOn: aCanvas length: length at: aPoint kern: kernDelta 
	| maskedString |
	maskedString := String new: length.
	maskedString atAllPut: substitutionCharacter.
	^ baseFont
		displayString: maskedString
		on: aCanvas
		from: 1
		to: length
		at: aPoint
		kern: kernDelta