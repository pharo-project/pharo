Extension { #name : #FixedLayout }

{ #category : #'*FluidClassDefinition' }
FixedLayout class >> extending: superLayout scope: aScope host: aClass [
	^ (superLayout extend: aScope)
		host: aClass;
		yourself
]

{ #category : #'*FluidClassDefinition' }
FixedLayout class >> kind [ 
	^ #normal
]
