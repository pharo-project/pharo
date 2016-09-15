tests
testTwoConnectedNodes
	"
	b
	|
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		}.
	
	expectedArray := { 
		HiNode contents: b. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a) }.
		
	self assertIteratedAsExpected.