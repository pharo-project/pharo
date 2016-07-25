geometry
extent: aPoint
	"Set the extent of the receiver.
	Update the pane color for size dependent fill/border styles."

	super extent: aPoint.
	self adoptPaneColor