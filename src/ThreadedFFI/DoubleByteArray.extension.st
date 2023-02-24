Extension { #name : #DoubleByteArray }

{ #category : #'*ThreadedFFI' }
DoubleByteArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
