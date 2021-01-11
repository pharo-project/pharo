Extension { #name : #FFIArchitecture }

{ #category : #'*ThreadedFFI-UFFI-Overrides' }
FFIArchitecture >> tfBitType [

	^ TFBasicType sint8
]

{ #category : #'*ThreadedFFI-UFFI-Overrides' }
FFIArchitecture >> tfUBitType [

	^ TFBasicType uint8
]

{ #category : #'*ThreadedFFI-UFFI-Overrides' }
FFIArchitecture >> tfexternalSizeTType [
	^ TFBasicType uint32
]
