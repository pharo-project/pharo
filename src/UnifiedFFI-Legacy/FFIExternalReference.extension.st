Extension { #name : #FFIExternalReference }

{ #category : #'*UnifiedFFI-Legacy' }
FFIExternalReference >> handle [
	^ self getHandle
]

{ #category : #'*UnifiedFFI-Legacy' }
FFIExternalReference >> handle: anObject [
	self setHandle: anObject
]
