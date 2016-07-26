selecting
selectFirst
	self
		assert: self candidates isEmpty not
		description: 'There must be at least one candidate in order to selected first'.
	
	self selected: self candidates head