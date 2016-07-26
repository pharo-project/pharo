buttons-event-handling
onPagePoped
	|button|
	
	button := self buttons removeLast.
	button announcer unsubscribe: self.
	
	self owner changed.
	self removeBrick: button.
	self updateButtons