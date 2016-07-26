private
isSearchingLiteralExpression: aMessageNode 
	| equalNode expressionNode |
	equalNode := aMessageNode selector = #| 
				ifTrue: [aMessageNode arguments first]
				ifFalse: [aMessageNode receiver].
	expressionNode := equalNode receiver isLiteralNode
				ifTrue: [equalNode arguments first]
				ifFalse: [equalNode receiver].
	^self isSearchingLiteralExpression: aMessageNode for: expressionNode