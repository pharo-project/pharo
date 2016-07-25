reflecive api
enable
	"turn on link reflectively if it has been turned off"
	self hasReifiedCondition
		ifTrue: [self condition enable].
	
