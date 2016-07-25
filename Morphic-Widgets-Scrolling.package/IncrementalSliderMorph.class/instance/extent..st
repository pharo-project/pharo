geometry
extent: aPoint
	"Set the button width to match the height."

	self extent = aPoint ifTrue: [^self].
	super extent: aPoint.
	self updateOrientation: aPoint