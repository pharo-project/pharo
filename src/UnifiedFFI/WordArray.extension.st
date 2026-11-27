Extension { #name : 'WordArray' }

{ #category : '*UnifiedFFI' }
WordArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
