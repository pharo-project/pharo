Extension { #name : #ClassDescription }

{ #category : #'*Ring-Definitions-Core' }
ClassDescription >> protocols [
1haltOnce.
	^ self organization protocolNames copy
]
