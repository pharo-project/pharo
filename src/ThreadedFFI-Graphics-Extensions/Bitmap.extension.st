Extension { #name : #Bitmap }

{ #category : #'*ThreadedFFI-Graphics-Extensions' }
Bitmap >> tfPointerAddress [

	^ PointerUtils oopForObject: self
]
