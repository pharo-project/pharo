Extension { #name : #DoubleWordArray }

{ #category : #'*ThreadedFFI' }
DoubleWordArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
