initialization
initialize

	super initialize.

	buttons := OrderedCollection new.
	canDeselectByClick := false asValueHolder.
	currentlyActivated := nil asValueHolder.
	
	self whenCanDeselectByClickChanged: [ :aBoolean | 
		buttons do: [ :each | each canDeselectByClick: aBoolean ]].