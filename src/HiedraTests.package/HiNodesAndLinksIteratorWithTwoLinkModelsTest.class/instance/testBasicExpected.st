tests
testBasicExpected
	"Secondary (i.e. origin) links are iterated after all primary (i.e. parents) links are iterated."

	"
	c
	|\\ 
	| || 
	| b 
	| | 
	|/ 
	a 	
	"
		
	reversedNodes := {
		a := HiTestNode content: $a parents: {}.
		b := HiTestNode content: $b parents: { a }.	
		c := (HiTestNode content: $c parents: { a }) origin: b; yourself
		}.
	
	expectedArray := {
		HiNode contents: c. 
		HiNode contents: a. 
		HiLink linkBuilder: parentsLinkBuilder origin: (HiNode contents: c) target: (HiNode contents: a). 
		HiNode contents: b. 
		HiLink linkBuilder: parentsLinkBuilder origin: (HiNode contents: b) target: (HiNode contents: a). 
		HiLink linkBuilder: originLinkBuilder  origin: (HiNode contents: c) target: (HiNode contents: b) }.
	
	self assertIteratedAsExpected.