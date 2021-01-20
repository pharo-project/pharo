Extension { #name : #IntegerArray }

{ #category : #'*ThreadedFFI' }
IntegerArray >> tfPointerAddress [

	self pinInMemory.
	^ PointerUtils oopForObject: self

]
