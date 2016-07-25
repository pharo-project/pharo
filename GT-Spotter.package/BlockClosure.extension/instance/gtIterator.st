*GT-Spotter
gtIterator
	" 1:filter 2:context[optional] "
	self assert: (self numArgs between: 1 and: 2). 
	
	^ GTSpotterBlockIterator new 
		onDo: self;
		yourself