Extension { #name : #'FFI_x86_64_Windows' }

{ #category : #'*ThreadedFFI-UFFI-Overrides' }
FFI_x86_64_Windows >> tfBitType [

	^ TFBasicType sint32
]

{ #category : #'*ThreadedFFI-UFFI-Overrides' }
FFI_x86_64_Windows >> tfUBitType [

	^ TFBasicType uint32
]
