tests - at put
testAtPutTwoValues
	"self debug: #testAtPutTwoValues"
	
	self nonEmpty at: self anIndex put: self aValue.
	self nonEmpty at: self anIndex put: self anotherValue.
	self assert: (self nonEmpty at: self anIndex) = self anotherValue.