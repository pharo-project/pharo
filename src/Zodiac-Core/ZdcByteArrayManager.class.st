"
I manage the allocation and recycling of ByteArrays. 

For each size, I maintain up to limit instances, configurable using #limit:

I never preallocate.

I am threadsafe.

My public API consists of just 2 messages: #byteArrayOfSize:zero: and #recyle:

There is one global current instance for me, but I can be used as needed.

	ZdcByteArrayManager current limit: 4.
	
	ZdcByteArrayManager current limit: 0.
"
Class {
	#name : #ZdcByteArrayManager,
	#superclass : #Object,
	#instVars : [
		'limit',
		'resources',
		'access'
	],
	#classVars : [
		'Current'
	],
	#category : #Zodiac-Core
}

{ #category : #cleanup }
ZdcByteArrayManager class >> cleanUp [
	self current resources removeAll
]

{ #category : #accessing }
ZdcByteArrayManager class >> current [
	^ Current ifNil: [ Current := self new ]
]

{ #category : #accessing }
ZdcByteArrayManager class >> current: anObject [
	Current := anObject
]

{ #category : #accessing }
ZdcByteArrayManager >> byteArrayOfSize: size zero: zero [
	"Return a ByteArray of size. If zero is true, make sure all bytes equal zero.
	When the byte array is no longer needed, #recycle can be used to return it to the reciever.
	It is possible that the byte array returned was previously recycled."
	
	limit = 0 ifTrue: [ ^ ByteArray new: size ].
	access critical: [ 
		| pool |
		pool := self poolForSize: size.
		"Look for an instance to reuse without allocation"
		pool withIndexDo: [ :value :index | 
			value ifNotNil: [ 
				pool at: index put: nil.
				zero ifTrue: [ value atAllPut: 0 ].
				^ value ] ].
		"Allocate a new instance "
		^ ByteArray new: size ]
]

{ #category : #initialize }
ZdcByteArrayManager >> initialize [
	super initialize.
	limit := 0.
	access := Monitor new
]

{ #category : #initialize }
ZdcByteArrayManager >> limit: newLimit [
	access critical: [ 
		limit := newLimit.
		limit = 0 ifTrue: [ ^ resources := nil ].
		resources ifNotNil: [ resources removeAll ] ]
]

{ #category : #private }
ZdcByteArrayManager >> poolForSize: size [
	^ self resources at: size ifAbsentPut: [ Array new: limit ]
]

{ #category : #accessing }
ZdcByteArrayManager >> recycle: byteArray [
	"Return byteArray, which is no longer needed, to the receiver to be recyled 
	and reused when #byteArrayOfSize:zero: is called."
	
	limit = 0 ifTrue: [ ^ self ].
	access critical: [ 
		| pool |
		pool := self poolForSize: byteArray size.
		"Find an empty slot in the pool"
		pool withIndexDo: [ :value :index | 
			value ifNil: [ 
				^ pool at: index put: byteArray ] ]
		"If the pool is full, don't hold onto it" ]
]

{ #category : #private }
ZdcByteArrayManager >> resources [
	^ resources ifNil: [ resources := Dictionary new ]
]

{ #category : #accessing }
ZdcByteArrayManager >> totalSize [
	^ resources 
		ifNil: [ 0 ] 
		ifNotNil: [ 
			(resources values collect: [ :pool | 
				pool
					inject: 0
					into: [ :sum :each | 
						sum + (each ifNil: [ 0 ] ifNotNil: [ each size ]) ] ]) sum ]
]
