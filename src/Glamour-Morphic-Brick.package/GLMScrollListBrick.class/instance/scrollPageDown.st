scrolling
scrollPageDown
	
	self
		scrollDown: self calculateMaxVelocity;
		update;
		changed