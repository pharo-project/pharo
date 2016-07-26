accessing
selectedColor: aColor
	"Set the value of selectedColor."

	selectedColor := aColor.
	self locationMorph visible ifTrue: [self updateSelectedLocation].
	self triggerEvent: #colorSelected with: aColor