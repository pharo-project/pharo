Extension { #name : 'DoubleByteArray' }

{ #category : '*UnifiedFFI' }
DoubleByteArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
