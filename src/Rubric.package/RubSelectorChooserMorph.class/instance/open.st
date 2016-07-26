ui-building
open
	super open. 
	self comeToFront.
	self borderStyle: ( self theme progressBarBorderStyleFor: self).
	self setPosition