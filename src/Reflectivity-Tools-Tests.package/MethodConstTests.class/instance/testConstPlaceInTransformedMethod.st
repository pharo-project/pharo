tests
testConstPlaceInTransformedMethod

	| transformedMethod const constLiteral |
	const := self constFromReceiverExpression. 
		
	transformedMethod := self class >> #constFromReceiverExpression. 
	
	constLiteral := transformedMethod literals detect: [ :each | each value == const ].
	self assert: (constLiteral key includesSubstring: 'meta' caseSensitive: false)