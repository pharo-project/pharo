Extension { #name : #FFIBit }

{ #category : #'*ThreadedFFI-UFFI' }
FFIBit >> tfExternalType [

	^ FFIArchitecture forCurrentArchitecture tfBitType
]
