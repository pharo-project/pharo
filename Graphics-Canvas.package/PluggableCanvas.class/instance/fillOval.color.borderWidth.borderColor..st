drawing-ovals
fillOval: r color: c borderWidth: borderWidth borderColor: borderColor
	self apply: [ :clippedCanvas |
		clippedCanvas fillOval: r color: c borderWidth: borderWidth borderColor: borderColor ]