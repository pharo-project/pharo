updating
changed

	| msRefreshRate isTimeForRefresh |
	msRefreshRate := 60 "roughly 16 times per second".
	isTimeForRefresh := Time millisecondClockValue - self lastRefresh >= msRefreshRate.
	(self isInWorld and: [ isTimeForRefresh ]) ifTrue: [ 
		super changed ].