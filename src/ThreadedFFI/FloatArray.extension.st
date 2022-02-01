Extension { #name : #FloatArray }

{ #category : #'*ThreadedFFI' }
FloatArray >> tfPointerAddress [

	^ PointerUtils oopForObject: self

]
