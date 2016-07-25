asserting
assertCommonAncestorOf: leftName and: rightName is: ancestorName in: tree
	self assertCommonAncestorOf: leftName and: rightName in: (Array with: ancestorName) in: tree