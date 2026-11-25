Extension { #name : 'Float32Array' }

{ #category : '*UnifiedFFI' }
Float32Array >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
