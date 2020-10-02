Extension { #name : #EphemeronLayout }

{ #category : #'*FluidClassDefinition' }
EphemeronLayout class >> extending: superLayout scope: aScope host: aClass [ 
	
	^ (superLayout extendEphemeron: aScope)
		host: aClass;
		yourself
]

{ #category : #'*FluidClassDefinition' }
EphemeronLayout class >> kind [ 
	^ #ephemeron
]
