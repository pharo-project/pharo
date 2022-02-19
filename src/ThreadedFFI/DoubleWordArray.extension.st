Extension { #name : #DoubleWordArray }

{ #category : #'*ThreadedFFI' }
DoubleWordArray >> tfPointerAddress [

	^ PointerUtils oopForObject: self

]
