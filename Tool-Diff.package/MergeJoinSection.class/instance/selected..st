accessing
selected: aBoolean
	"Set the value of selected"

	selected := aBoolean.
	self
		updateHighlights;
		changed: #selected