Extension { #name : 'ByteString' }

{ #category : '*UnifiedFFI' }
ByteString >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
