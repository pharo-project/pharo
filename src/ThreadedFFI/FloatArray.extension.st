Extension { #name : #FloatArray }

{ #category : #'*ThreadedFFI' }
FloatArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self

]
