tests - at put
testAtPutOutOfBounds
	"self debug: #testAtPutOutOfBounds"
	
	self should: [self empty at: self anIndex put: self aValue] raise: Error
	