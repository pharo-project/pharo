actions
click: evt
	super click: evt.
	
	self stepModel spotter popUntil: self stepModel.
	^ true