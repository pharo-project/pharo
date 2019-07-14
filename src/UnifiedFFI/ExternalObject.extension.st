Extension { #name : #ExternalObject }

{ #category : #'*UnifiedFFI' }
ExternalObject >> autoRelease [
	self isExternalAddress ifFalse: [ ^ self ].
	^ self getHandle autoRelease
]

{ #category : #'*UnifiedFFI' }
ExternalObject >> pointer [ 
	^ self getHandle pointer
]
