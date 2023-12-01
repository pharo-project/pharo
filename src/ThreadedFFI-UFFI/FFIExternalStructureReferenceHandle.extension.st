Extension { #name : #FFIExternalStructureReferenceHandle }

{ #category : #'*ThreadedFFI-UFFI' }
FFIExternalStructureReferenceHandle >> tfPointerAddress [

	^ handle tfPointerAddress + startOffset
]
