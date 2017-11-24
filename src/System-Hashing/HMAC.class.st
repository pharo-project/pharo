"
HMAC is a mechanism for message authentication using cryptographic hash functions. HMAC can be used with any iterative cryptographic hash function, e.g., MD5, SHA-1, in combination with a secret shared key.  The cryptographic strength of HMAC depends on the properties of the underlying hash function.

See RFC 2114.
"
Class {
	#name : #HMAC,
	#superclass : #Object,
	#instVars : [
		'hash',
		'key',
		'ipad',
		'epad'
	],
	#category : #'System-Hashing-Core'
}

{ #category : #'instance creation' }
HMAC class >> on: aHashFunction [
	^ self new setHash: aHashFunction
]

{ #category : #accessing }
HMAC >> destroy [
	key destroy
]

{ #category : #accessing }
HMAC >> digestMessage: aByteArray [
	^ hash hashMessage: (key bitXor: epad), (hash hashMessage: (key bitXor: ipad), aByteArray)
]

{ #category : #accessing }
HMAC >> digestSize [
	^ hash hashSize
]

{ #category : #accessing }
HMAC >> key: aByteArray [ 
	key := aByteArray.
	key size > hash blockSize ifTrue: [ key := hash hashMessage: key ].
	key size < hash blockSize ifTrue: [ key := key , (ByteArray new: hash blockSize - key size) ]
]

{ #category : #printing }
HMAC >> printOn: aStream [
	aStream nextPutAll: 'HMAC-'; print: hash
]

{ #category : #initialization }
HMAC >> setHash: aHash [ 
	hash := aHash.
	ipad := ByteArray 
		new: aHash blockSize
		withAll: 54.
	epad := ByteArray 
		new: aHash blockSize
		withAll: 92
]
