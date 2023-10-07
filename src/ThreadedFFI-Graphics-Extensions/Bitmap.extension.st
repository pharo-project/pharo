Extension { #name : 'Bitmap' }

{ #category : '*ThreadedFFI-Graphics-Extensions' }
Bitmap >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
