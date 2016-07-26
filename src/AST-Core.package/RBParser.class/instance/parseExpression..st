accessing
parseExpression: aString 
	| node |
	node := self parseStatements: false.
	(RBMethodNode selector: #noMethod body: node) source: aString.	"Make the sequence node have a method node as its parent"
	self atEnd ifFalse: [ ^ self parseIncompleteExpression: node ].
	^node