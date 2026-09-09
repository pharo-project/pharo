Extension { #name : 'FFIArchitecture' }

{ #category : '*ThreadedFFI-UFFI' }
FFIArchitecture >> tfBitType [

	^ TFBasicType sint8
]

{ #category : '*ThreadedFFI-UFFI' }
FFIArchitecture >> tfUBitType [

	^ TFBasicType uint8
]

{ #category : '*ThreadedFFI-UFFI' }
FFIArchitecture >> tfexternalSizeTType [
	^ TFBasicType uint32
]
