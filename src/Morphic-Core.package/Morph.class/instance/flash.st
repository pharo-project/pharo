macpal
flash
	| originalFill flashFill |
	
	flashFill := self flashFillStyle.
	originalFill := self fillStyle.
	
	self fillStyle: flashFill.
	
	self addAlarm: #flashFinished:original: withArguments: { flashFill . originalFill } after: 50