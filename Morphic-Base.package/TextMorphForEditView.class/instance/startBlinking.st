blinking
startBlinking
	self canChangeText 
		ifTrue: [super startBlinking]
		ifFalse: [self paragraph showCaret: true]