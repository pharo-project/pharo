private
refresh

	lastRefresh := Time millisecondClockValue.
	self currentWorld doOneCycleNow.