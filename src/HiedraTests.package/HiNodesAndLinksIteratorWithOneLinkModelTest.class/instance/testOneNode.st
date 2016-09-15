tests
testOneNode
	"
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		}.
	
	expectedArray := { HiNode contents: a }. 
		
	self assertIteratedAsExpected.