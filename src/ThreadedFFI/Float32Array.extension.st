Extension { #name : #Float32Array }

{ #category : #'*ThreadedFFI' }
Float32Array >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
