private
buildTree: aString method: aBoolean 
	^aBoolean 
		ifTrue: [RBParser parseRewriteMethod: aString]
		ifFalse: [RBParser parseRewriteExpression: aString]