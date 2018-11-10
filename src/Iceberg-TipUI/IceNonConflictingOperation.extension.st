Extension { #name : #IceNonConflictingOperation }

{ #category : #'*Iceberg-TipUI' }
IceNonConflictingOperation >> operationIcon [
	
	^ operation icon
]

{ #category : #'*Iceberg-TipUI' }
IceNonConflictingOperation >> preferredColor [
	
	^ self isResolved
		ifTrue: [ Color gray ]
		ifFalse: [ nil ]
]
