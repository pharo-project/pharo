Extension { #name : #ExternalObject }

{ #category : #'*ThreadedFFI' }
ExternalObject >> tfPointerAddress [

	^ self getHandle tfPointerAddress
]
