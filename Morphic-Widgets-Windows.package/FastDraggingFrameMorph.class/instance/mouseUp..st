event handling
mouseUp: evt 
	target
		ifNotNil: [Display
				deferUpdatesIn: Display boundingBox
				while: [target bounds: self bounds]].
	self delete