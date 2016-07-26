initialization
initialize
	
	super initialize.
	cache := ReCriticsCache new.
	checker := ReSmalllintChecker new.
	cache checker: checker.
	cache browser: self.
	criticModel cache: cache.