Extension { #name : 'ByteArray' }

{ #category : '*Random-Core' }
ByteArray >> generateUUIDInPlace [

	<primitive: 'primitiveMakeUUID' module: 'UUIDPlugin'>
	self size = 16 ifFalse: [ self error: 'The byte array size has to be 16 bytes.' ].
	self primitiveFailed
]
