Extension { #name : #IceConflictingOperation }

{ #category : #'*Iceberg-TipUI' }
IceConflictingOperation >> operationIcon [
	^ self iconNamed: #changeUpdate
]

{ #category : #'*Iceberg-TipUI' }
IceConflictingOperation >> preferredColor [
	
	^ self isResolved
		ifTrue: [ Color gray ]
		ifFalse: [ Color red ]
]
