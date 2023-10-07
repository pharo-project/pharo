Extension { #name : 'ExternalObject' }

{ #category : '*UnifiedFFI' }
ExternalObject >> autoRelease [
	self getHandle isExternalAddress ifFalse: [ ^ self ].
	self getHandle autoRelease.
	^ self
]

{ #category : '*UnifiedFFI' }
ExternalObject >> pointer [
	^ self getHandle pointer
]
