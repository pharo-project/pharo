Extension { #name : #FFILibrary }

{ #category : #'*ThreadedFFI-UFFI' }
FFILibrary >> callbackRunningStrategy [

	^ TFCallbackForkRunStrategy new
]

{ #category : #'*ThreadedFFI-UFFI' }
FFILibrary >> runner [

	^ TFSameThreadRunner uniqueInstance
]
