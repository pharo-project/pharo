Extension { #name : #ByteArray }

{ #category : #'*ThreadedFFI' }
ByteArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self

]
