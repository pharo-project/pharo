as yet unclassified
click: anEvent
	super click: anEvent.
	
	self announcer announce: (GLMPagerButtonClicked new buttonModel: self)
	