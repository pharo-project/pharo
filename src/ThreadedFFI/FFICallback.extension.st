Extension { #name : #FFICallback }

{ #category : #'*ThreadedFFI' }
FFICallback >> tfPointerAddress [

	^ self getHandle tfPointerAddress
]

{ #category : #'*ThreadedFFI' }
FFICallback >> tfPrintString [

	^ block printString
]
