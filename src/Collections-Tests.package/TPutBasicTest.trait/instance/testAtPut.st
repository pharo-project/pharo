tests - at put
testAtPut
	"self debug: #testAtPut"
	
	self nonEmpty at: self anIndex put: self aValue.
	self assert: (self nonEmpty at: self anIndex) = self aValue.
	