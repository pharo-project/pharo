accessing
stepModel: anObject
	stepModel := anObject.
	
	self stepModel spotter announcer when: GTSpotterRevealHints send: #onRevealHints to: self.
	self stepModel spotter announcer when: GTSpotterHideHints send: #onHideHints to: self.
	
	self
		addActActionButton;
		addDiveInActionButton