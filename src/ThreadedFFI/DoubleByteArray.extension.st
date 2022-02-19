Extension { #name : #DoubleByteArray }

{ #category : #'*ThreadedFFI' }
DoubleByteArray >> tfPointerAddress [

	^ PointerUtils oopForObject: self

]
