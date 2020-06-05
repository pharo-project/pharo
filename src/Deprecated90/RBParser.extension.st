Extension { #name : #RBParser }

{ #category : #'*Deprecated90' }
RBParser class >> parseCompiledMethod: aCompiledMethod [
	self 
		deprecated: 'use #parseMethod:'
		transformWith:  '`@receiver parseCompiledMethod: `@arg' -> '`@receiver parseMethod: `@arg' sourceCode.
	^ self parseMethod: aCompiledMethod sourceCode
]
