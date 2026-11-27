Extension { #name : 'IntegerArray' }

{ #category : '*UnifiedFFI' }
IntegerArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
