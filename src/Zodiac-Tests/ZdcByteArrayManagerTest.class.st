"
ZdcByteArrayManagerTests holds unit tests for ZdcByteArrayManager
"
Class {
	#name : #ZdcByteArrayManagerTest,
	#superclass : #TestCase,
	#category : #'Zodiac-Tests'
}

{ #category : #testing }
ZdcByteArrayManagerTest >> testClearing [
	| byteArrayManager one two |
	byteArrayManager := ZdcByteArrayManager new.
	byteArrayManager limit: 1.
	self assert: byteArrayManager totalSize equals: 0.
	one := (1 to: 10) asByteArray.
	byteArrayManager recycle: one.
	two := byteArrayManager byteArrayOfSize: 10 zero: true.
	self assert: one identicalTo: two.
	self assert: two equals: (ByteArray new: 10)
]

{ #category : #testing }
ZdcByteArrayManagerTest >> testSimple [
	| byteArrayManager byteArray |
	byteArrayManager := ZdcByteArrayManager new.
	byteArrayManager limit: 1.
	self assert: byteArrayManager totalSize equals: 0.
	byteArray := byteArrayManager byteArrayOfSize: 4096 zero: false.
	byteArrayManager recycle: byteArray.
	self assert: byteArrayManager totalSize equals: 4096.
	self assert: (byteArrayManager byteArrayOfSize: 4096 zero: false) identicalTo: byteArray
]
