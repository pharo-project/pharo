Extension { #name : #ByteString }

{ #category : #'*ThreadedFFI' }
ByteString >> tfPointerAddress [

	^ PointerUtils oopForObject: self

]
