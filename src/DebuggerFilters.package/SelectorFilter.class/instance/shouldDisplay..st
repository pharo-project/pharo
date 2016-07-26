testing
shouldDisplay: aContext
	 ^ (self selectors  
		includes: aContext method selector) not.