API
changed

	| msRefreshRate isTimeForRefresh |
	msRefreshRate := 25.
	isTimeForRefresh := Time millisecondClockValue - lastRefresh >= msRefreshRate.
	super changed.
	(self isInWorld and: [ isTimeForRefresh ]) ifTrue: [ self refresh ].