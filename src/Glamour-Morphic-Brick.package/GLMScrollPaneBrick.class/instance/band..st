scrollpane-brick-accessing
band: aBrick

	band := aBrick.
	self band scrollpane: self.
	self band hSpaceFill.
	super addBrickBack: self band.