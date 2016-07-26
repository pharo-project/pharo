instance creation
value: aValue 
	^(self new)
		startPosition: 0
			contents: (aValue asArray collect: [:each | RBLiteralNode value: each])
			stopPosition: -1
			isByteArray: aValue class ~~ Array;
		yourself