tests
testCommonAncestors
	self assertCommonAncestorOf: #a2 and: #e2 is: #a1 in: self tree.
	self assertCommonAncestorOf: #e2 and: #b3 is: #a1 in: self tree.
	self assertCommonAncestorOf: #b2 and: #e2 is: #'00' in: self tree.
	
	self assertCommonAncestorOf: #a4 and: #b5 in: #(b2 a1) in: self twoPersonTree.
	self assertCommonAncestorOf: #b5 and: #b3 is: #b2 in: self twoPersonTree.
	self assertCommonAncestorOf: #b2 and: #a4 is: #b2 in: self twoPersonTree.
	self assertCommonAncestorOf: #b2 and: #b2 is: #b2 in: self twoPersonTree.
	self assertCommonAncestorOf: #b2 and: #a1 is: #a1 in: self twoPersonTree.
	self assertCommonAncestorOf: #a1 and: #b2 is: #a1 in: self twoPersonTree.