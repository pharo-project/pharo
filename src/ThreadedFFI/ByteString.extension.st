Extension { #name : #ByteString }

{ #category : #'*ThreadedFFI' }
ByteString >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
