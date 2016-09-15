tests
testTwoUnmergedForks
	"
	c
	|
	|
	| b
	| |
	|/
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := HiTestNode content: $c parents: { a }.	
		}.
	
	expectedArray := { 
		HiNode contents: c. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: a). 
		HiNode contents: b. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). }.
		
	self assertIteratedAsExpected.