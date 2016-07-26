testing
hasMethodBeenRun: aSelector
	^ ((self lastRun at: #errors),
		(self lastRun at: #failures),
		(self lastRun at: #passed))
			includes: aSelector