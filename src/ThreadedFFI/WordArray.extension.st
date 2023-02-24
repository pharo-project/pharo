Extension { #name : #WordArray }

{ #category : #'*ThreadedFFI' }
WordArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
