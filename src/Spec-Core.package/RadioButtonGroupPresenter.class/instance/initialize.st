initialization
initialize
	buttons := OrderedCollection new.
	canDeselectByClick := false asValueHolder.
	currentActivated := nil asValueHolder.
	super initialize.