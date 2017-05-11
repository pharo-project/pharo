accessing
adoptPaneColor: paneColor
	"Change our border color too."
	
	|c|
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	c := paneColor alphaMixed: 0.1 with: self theme diffMapColor.
	self fillStyle: ((GradientFillStyle
			ramp: (self gradientRampForColor: c))
		origin: self bounds topLeft;
		direction: 0@ self height).
	self borderStyle baseColor: paneColor