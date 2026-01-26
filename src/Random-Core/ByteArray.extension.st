Extension { #name : 'ByteArray' }

{ #category : '*Random-Core' }
ByteArray >> generateUUIDInPlace [

	<primitive: 'primitiveMakeUUID' module: 'UUIDPlugin'>
	self size = 16 ifFalse: [ self error: 'The byte array size has to be 16 bytes.' ].
	"We should remove the next line around the moment we release Pharo 14"
	self error: ('Cannot generate UUID. It requires at least version 10.3.9 of the Pharo vm. Current version: ', Smalltalk vm interpreterSourceVersion).
	self primitiveFailed
]
