Extension { #name : 'ExternalData' }

{ #category : '*UnifiedFFI' }
ExternalData >> + offset [
	"Pointer aritmetic, it allows to move the pointer as in C.
	 But obviously, it only applies to actual external addresses."
	self getHandle isExternalAddress
		ifFalse: [ self error: 'You cannot apply pointer aritmetic to a non external address' ].
	^ self species
		fromHandle: (self getHandle + offset)
		type: type
]

{ #category : '*UnifiedFFI' }
ExternalData >> asInteger [
	^ self getHandle asInteger
]

{ #category : '*UnifiedFFI' }
ExternalData >> beNull [
	^ self getHandle beNull
]

{ #category : '*UnifiedFFI' }
ExternalData >> copyFrom: start to: stop [
	^ self getHandle copyFrom: start to: stop
]

{ #category : '*UnifiedFFI' }
ExternalData >> value [
	^ self asInteger
]
