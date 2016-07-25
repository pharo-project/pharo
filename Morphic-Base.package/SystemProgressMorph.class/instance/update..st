updating
update: aSymbol
	
	| msRefreshRate isTimeForRefresh |
	
	aSymbol == #width
		ifTrue: [ self updateWidth ].
		
	
	msRefreshRate := 60 "roughly 16 times per second".
	isTimeForRefresh := Time millisecondClockValue - self lastRefresh >= msRefreshRate.
	(self isInWorld and: [ isTimeForRefresh ]) ifFalse: [ ^ self ].
	
	self refresh