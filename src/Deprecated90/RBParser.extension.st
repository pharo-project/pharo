Extension { #name : #RBParser }

{ #category : #'*Deprecated90' }
RBParser class >> parseCompiledMethod: aCompiledMethod [
	self 
		deprecated: 'use #parseMethod:'
		transformWith:  '`@receiver parseCompiledMethod: `@arg' -> '`@receiver parseMethod: `@arg' sourceCode.
	^ self parseMethod: aCompiledMethod sourceCode
]

{ #category : #'*Deprecated90' }
RBParser >> parseExpression: anExpressionString [
	self 
		deprecated: 'Use #parseExpression without argument' 
		transformWith: '`@receiver parseExpression: `@arg' -> '`@receiver parseExpression'.
		
	^self parseExpression  
]

{ #category : #'*Deprecated90' }
RBParser >> parseMethod: aSourseString [
	self 
		deprecated: 'Use #parseMethod without argument' 
		transformWith: '`@receiver parseMethod: `@arg' -> '`@receiver parseMethod'.
		
	^self parseMethod 
]
