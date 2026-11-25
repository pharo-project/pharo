Extension { #name : 'Bitmap' }

{ #category : '*UnifiedFFI' }
Bitmap >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
