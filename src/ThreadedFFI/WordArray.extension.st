Extension { #name : #WordArray }

{ #category : #'*ThreadedFFI' }
WordArray >> tfPointerAddress [

	^ PointerUtils oopForObject: self

]
