accessing
autoSelectionColor: anObject
	"Set the value of autoSelectionColor"

	autoSelectionColor := anObject.
	self
		changed: #autoSelectionColor;
		changed: #manualSelectionColor;
		changed: #selectionColor