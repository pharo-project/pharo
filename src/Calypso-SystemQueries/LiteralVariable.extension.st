Extension { #name : #LiteralVariable }

{ #category : #'*Calypso-SystemQueries' }
LiteralVariable >> isReferencedIn: aMethod [ 
	
	^(self isReadIn: aMethod) or: [ self isWrittenIn: aMethod ]
]

{ #category : #'*Calypso-SystemQueries' }
LiteralVariable >> printAsConstantQueryItem [
	^key
]
