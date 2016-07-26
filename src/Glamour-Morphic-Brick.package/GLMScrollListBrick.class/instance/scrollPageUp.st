scrolling
scrollPageUp
	
	self
		scrollUp: self calculateMaxVelocity;
		update;
		changed