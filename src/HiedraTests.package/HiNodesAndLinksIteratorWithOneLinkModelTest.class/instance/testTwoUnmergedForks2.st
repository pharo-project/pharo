tests
testTwoUnmergedForks2
	"
	d
	|
	| c
	| |
	b |
	|/
	a
	"

	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := HiTestNode content: $c parents: { a }.	
		d := HiTestNode content: $d parents: { b }.	
		}.
	
	expectedArray := { 
		HiNode contents: d. 
		HiNode contents: b. 
		HiNode contents: a. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: d) target: (HiNode contents: b). 
		HiNode contents: c. 
		HiLink linkBuilder: linkBuilder origin: (HiNode contents: c) target: (HiNode contents: a) }.
		
	self assertIteratedAsExpected.