Extension { #name : 'Behavior' }

{ #category : '*ThreadedFFI-UFFI' }
Behavior >> basicNewPinned [

	<primitive: 598 error: ec>

	^ self basicNew
		pinInMemory;
		yourself
]

{ #category : '*ThreadedFFI-UFFI' }
Behavior >> basicNewPinned: requestedSize [

	<primitive: 599 error: ec>

	^ (self basicNew: requestedSize)
		pinInMemory;
		yourself
]

{ #category : '*ThreadedFFI-UFFI' }
Behavior >> newPinned [

	^ ObjectAllocator value: PinnedAllocator during: [ self new ]
]

{ #category : '*ThreadedFFI-UFFI' }
Behavior >> newPinned: requestedSize [

	^ ObjectAllocator
		  value: PinnedAllocator
		  during: [ self new: requestedSize ]
]
