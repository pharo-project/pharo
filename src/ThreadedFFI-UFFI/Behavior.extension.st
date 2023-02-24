Extension { #name : #Behavior }

{ #category : #'*ThreadedFFI-UFFI' }
Behavior >> basicNewPinned [

	<primitive: 598 error: ec>

	^ self basicNew
		pinInMemory;
		yourself
]

{ #category : #'*ThreadedFFI-UFFI' }
Behavior >> basicNewPinned: requestedSize [

	<primitive: 599 error: ec>

	^ (self basicNew: requestedSize)
		pinInMemory;
		yourself
]

{ #category : #'*ThreadedFFI-UFFI' }
Behavior >> newPinned [

	^ self basicNewPinned initialize
]

{ #category : #'*ThreadedFFI-UFFI' }
Behavior >> newPinned: sizeRequested [

	^ (self basicNewPinned: sizeRequested) initialize
]
