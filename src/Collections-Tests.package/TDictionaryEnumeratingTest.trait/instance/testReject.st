tests - dictionnary enumerating
testReject
	"Ensure that Dictionary>>reject: answers a dictionary not something else"
	
	| collection result |
	collection := self nonEmptyDict .
	result := collection reject: [ :each | false].
	
	self assert: result = collection. 