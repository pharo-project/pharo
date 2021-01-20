Extension { #name : #Bitmap }

{ #category : #'*ThreadedFFI' }
Bitmap >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self
]
