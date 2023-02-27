Extension { #name : #FFIUBit }

{ #category : #'*ThreadedFFI-UFFI' }
FFIUBit >> tfExternalType [

	^ FFIArchitecture forCurrentArchitecture tfUBitType
]
