visiting
visitLiteralArrayNode: aRBLiteralArrayNode
	"in a (valid) byte array all elements are of the same type, style the whole contents
at once, but for ordinary literal arrays, style every node"

	(aRBLiteralArrayNode isForByteArray and: [ 
		aRBLiteralArrayNode isFaulty not and: [ 
			aRBLiteralArrayNode contents isNotEmpty 
		]
	]) 
	ifTrue: [ 
		self addStyle: #number 
			from: aRBLiteralArrayNode start + 2 
			to: aRBLiteralArrayNode stop - 1
	]
	ifFalse: [ aRBLiteralArrayNode contents do: [ :each | self visitNode: each ] ]