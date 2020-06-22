Extension { #name : #Variable }

{ #category : #'*OpalCompiler-Core' }
Variable >> semanticNodeClass [
	^self class semanticNodeClass
]

{ #category : #'*OpalCompiler-Core' }
Variable class >> semanticNodeClass [
	^RBVariableNode
]
