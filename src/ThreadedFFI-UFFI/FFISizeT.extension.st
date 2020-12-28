Extension { #name : #FFISizeT }

{ #category : #'*ThreadedFFI-UFFI' }
FFISizeT >> tfExternalType [

	^ FFIArchitecture forCurrentArchitecture tfexternalSizeTType
]
