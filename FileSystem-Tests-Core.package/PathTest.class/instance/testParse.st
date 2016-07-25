tests
testParse
	"self run: #testParse"
	
	| path |
	path := Path from: 'parent/child/grandChild' delimiter: $/.
	self assert: path size equals: 3.
	self assert: (path at: 1) equals: 'parent'.
	self assert: (path at: 2) equals: 'child'.
	self assert: (path at: 3) equals: 'grandChild'.
	
