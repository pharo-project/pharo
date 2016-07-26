accessing
windowColor: anObject
	"Set the value of windowColor"

	windowColor := anObject.
	self changed: #windowColor.
	self autoSelectionColor ifTrue: [self changed: #selectionColor]