tests
testTwoUnconnectedNodes
	"
	b

	a	 	
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: {}.	
		}.
	
	expectedArray := { 
		HiNode contents: b. 
		HiNode contents: a. }.
		
	self assertIteratedAsExpected.