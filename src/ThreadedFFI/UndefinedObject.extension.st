Extension { #name : #UndefinedObject }

{ #category : #'*ThreadedFFI' }
UndefinedObject >> pinStructHandleInMemory [

	"Do not change pinning of nil. Return its current pinned state"
	^ self isPinnedInMemory
]

{ #category : #'*ThreadedFFI' }
UndefinedObject >> tfPointerAddress [

	^ ExternalAddress null
]
