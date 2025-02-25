"
I am ZnByteArrayEntity, a concrete HTTP Entity based on a ByteArray.
It is used to hold binary (non textual) data.
I am a ZnEntity.

Part of Zinc HTTP Components.
"
Class {
	#name : 'ZnByteArrayEntity',
	#superclass : 'ZnEntity',
	#instVars : [
		'bytes'
	],
	#category : 'Zinc-HTTP-Core',
	#package : 'Zinc-HTTP',
	#tag : 'Core'
}

{ #category : 'instance creation' }
ZnByteArrayEntity class >> bytes: byteArray [
	^ (self type: ZnMimeType applicationOctetStream length: byteArray size)
		bytes: byteArray;
		yourself
]

{ #category : 'testing' }
ZnByteArrayEntity class >> designatedMimeType [
	"I have no designated mime type, I can handle all binary content.
	See #concreteSubclassForType: "

	^ nil
]

{ #category : 'testing' }
ZnByteArrayEntity class >> matches: mimeType [
	"I have no designated mime type, I can handle all binary content.
	See #concreteSubclassForType: "

	^ false
]

{ #category : 'comparing' }
ZnByteArrayEntity >> = other [
	^ super = other and: [ self bytes = other bytes ]
]

{ #category : 'accessing' }
ZnByteArrayEntity >> bytes [
	^ bytes
]

{ #category : 'accessing' }
ZnByteArrayEntity >> bytes: anObject [
	bytes := anObject
]

{ #category : 'accessing' }
ZnByteArrayEntity >> contents [
	^ self bytes
]

{ #category : 'comparing' }
ZnByteArrayEntity >> hash [
	^ super hash bitXor: self bytes hash
]

{ #category : 'testing' }
ZnByteArrayEntity >> isEmpty [
	^ self bytes isNil or: [ self bytes isEmpty ]
]

{ #category : 'printing' }
ZnByteArrayEntity >> printContentsOn: stream [

	super printContentsOn: stream.
	self bytes ifNotNil: [
		stream
			space;
			print: self bytes ]
]

{ #category : 'initialization' }
ZnByteArrayEntity >> readFrom: stream [
	self contentLength
		ifNil: [
			self bytes: (ZnUtils readUpToEnd: stream limit: (ZnCurrentOptions at: #maximumEntitySize)).
			self contentLength: self bytes size ]
		ifNotNil: [ | byteArray readCount |
			self contentLength > (ZnCurrentOptions at: #maximumEntitySize)
				ifTrue: [ ZnEntityTooLarge signal ].
			byteArray := ByteArray ofSize: self contentLength.
			readCount := self contentLength > ZnUtils streamingBufferSize
				ifTrue: [ ZnUtils streamFrom: stream to: byteArray writeStream size: self contentLength ]
				ifFalse: [ stream readInto: byteArray startingAt: 1 count: self contentLength ].
			readCount = self contentLength 
				ifTrue: [ self bytes: byteArray ]
				ifFalse: [ self bytes: (byteArray copyFrom: 1 to: readCount); contentLength: readCount ] ]
]

{ #category : 'accessing' }
ZnByteArrayEntity >> readStream [
	^ self bytes readStream
]

{ #category : 'writing' }
ZnByteArrayEntity >> writeOn: stream [
	ZnUtils nextPutAll: self bytes on: stream
]
