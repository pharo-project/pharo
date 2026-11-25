Extension { #name : 'Integer' }

{ #category : '*UnifiedFFI' }
Integer >> asExternalTypeOn: generator [

	^ FFIConst
		value: self
		type: (generator resolveType: #int32)
]

{ #category : '*UnifiedFFI' }
Integer >> pointer [
	^ (ByteArray new: FFIExternalType pointerSize)
		unsignedLongAt: 1 put: self;
		yourself
]

{ #category : '*UnifiedFFI' }
Integer >> tfPointerAddress [

    ^ExternalAddress new fromInteger: self
]
