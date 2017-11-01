Extension { #name : #KomitClass }

{ #category : #'*Komitter-UI' }
KomitClass >> addClassDefinition: aMCRemoval [ 

	self definition: aMCRemoval
]

{ #category : #'*Komitter-UI' }
KomitClass >> addMethodDefinition: aMCAddition [

	self addOperation: aMCAddition
]

{ #category : #'*Komitter-UI' }
KomitClass >> addOrganizationDefinition: aMCModification [ 
	
	self addOperation: aMCModification
]

{ #category : #'*Komitter-UI' }
KomitClass >> koDestinationText [

	^ self isDirty
		ifTrue: [ self definition koDestinationText ]
		ifFalse: [ '' ]
]

{ #category : #'*Komitter-UI' }
KomitClass >> koSourceText [

	^ self isDirty
		ifFalse: [ 'No changes' ]
		ifTrue: [ self definition koSourceText ]
]
