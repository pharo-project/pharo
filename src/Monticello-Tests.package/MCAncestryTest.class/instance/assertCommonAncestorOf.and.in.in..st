asserting
assertCommonAncestorOf: leftName and: rightName in: options in: tree
	| left right ancestor |
	left := self versionForName: leftName in: tree.
	right := self versionForName: rightName in: tree.
	
	ancestor := left commonAncestorWith: right.
	
	self assert: (options includes: ancestor name)