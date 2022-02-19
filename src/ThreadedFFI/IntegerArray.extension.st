Extension { #name : #IntegerArray }

{ #category : #'*ThreadedFFI' }
IntegerArray >> tfPointerAddress [

	^ PointerUtils oopForObject: self

]
