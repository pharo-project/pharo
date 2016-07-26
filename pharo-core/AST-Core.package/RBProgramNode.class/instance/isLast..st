testing
isLast: aNode 
	| children |
	children := self children.
	^children notEmpty and: [children last == aNode]