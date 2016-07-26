geometry
extent: aPoint
	"Set the extent of the receiver.
	Update the tabs."

	self extent = aPoint ifTrue: [^self].
	super extent: aPoint.
	self
		updateTabs;
		ensureSelectedTabIsVisible