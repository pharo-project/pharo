tests
testTwoMergedFork
	"
	c
	|\ 
	| | 
	b | 
	| | 
	|/ 
	a 	
	"
		
	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := HiTestNode content: $c parents: { a. b } 
		}.
	
	expectedArray := { 
		HiNode contents: c. 
		HiNode contents: b. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: b). 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: a) }.
	
	self assertIteratedAsExpected.