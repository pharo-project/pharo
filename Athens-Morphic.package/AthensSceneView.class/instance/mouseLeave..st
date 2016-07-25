event handling
mouseLeave: evt
	self 
		resetState: #mouseIn;
		resetState: #panning.