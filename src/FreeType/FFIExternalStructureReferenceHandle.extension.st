Extension { #name : #FFIExternalStructureReferenceHandle }

{ #category : #'*FreeType' }
FFIExternalStructureReferenceHandle >> asExternalAddress [
	^ handle + startOffset
]
