visiting
visitLiteralArrayNode: aRBLiteralArrayNode
	"in a (valid) byte array all elements are of the same type, style the whole contents
at once, but for ordinary literal arrays, style every node"

	(aRBLiteralArrayNode isForByteArray and: [ aRBLiteralArrayNode isFaulty not ])
		ifTrue: [ self addStyle: #literalArray forNode: aRBLiteralArrayNode ]
		ifFalse: [ aRBLiteralArrayNode contents do: [ :each | self visitNode: each ] ]