"
Common superclass for hash functions
"
Class {
	#name : #HashFunction,
	#superclass : #Object,
	#category : #'System-Hashing-Core'
}

{ #category : #accessing }
HashFunction class >> blockSize [
	^ self subclassResponsibility
]

{ #category : #hashing }
HashFunction class >> hashMessage: aStringOrByteArray [
	^ self new hashMessage: aStringOrByteArray
]

{ #category : #accessing }
HashFunction class >> hashSize [
	^ self subclassResponsibility
]

{ #category : #hashing }
HashFunction class >> hashStream: aPositionableStream [
	^ self new hashStream: aPositionableStream
]

{ #category : #accessing }
HashFunction >> blockSize [
	^ self class blockSize
]

{ #category : #accessing }
HashFunction >> doubleHashMessage: aStringOrByteArray [
	"SHA1 new doubleHashMessage: 'foo'"
	^ self doubleHashStream: aStringOrByteArray asByteArray readStream
]

{ #category : #accessing }
HashFunction >> doubleHashStream: aStream [
	^ self hashStream: ((self hashStream: aStream) asByteArray readStream)
]

{ #category : #accessing }
HashFunction >> hashMessage: aStringOrByteArray [
	"MD5 new hashMessage: 'foo'"
	^ self hashStream: aStringOrByteArray asByteArray readStream
]

{ #category : #accessing }
HashFunction >> hashSize [
	^ self class hashSize
]

{ #category : #accessing }
HashFunction >> hashStream: aStream [
	^ self subclassResponsibility
]

{ #category : #converting }
HashFunction >> hmac [
	^ HMAC on: self
]
