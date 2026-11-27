Extension { #name : 'DoubleWordArray' }

{ #category : '*UnifiedFFI' }
DoubleWordArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
