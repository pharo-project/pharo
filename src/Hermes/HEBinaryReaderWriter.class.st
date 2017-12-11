Class {
	#name : #HEBinaryReaderWriter,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : 'Hermes'
}

{ #category : #sizes }
HEBinaryReaderWriter >> byteArraySize: aByteArray [
	^ self intSize + aByteArray size.

]

{ #category : #sizes }
HEBinaryReaderWriter >> byteStringSize: aString [
	^ self byteArraySize: aString asArray

]

{ #category : #'initialize-release' }
HEBinaryReaderWriter >> close [
	self stream close.
]

{ #category : #accessing }
HEBinaryReaderWriter >> flush [
	stream flush
]

{ #category : #sizes }
HEBinaryReaderWriter >> intSize [
	^ 4
]

{ #category : #reading }
HEBinaryReaderWriter >> readByte [
	^ stream next
]

{ #category : #reading }
HEBinaryReaderWriter >> readByteArray [
	| byteArray size |
	size := self readInt32. 
	byteArray := stream next: size.
	^ byteArray
]

{ #category : #reading }
HEBinaryReaderWriter >> readByteString [
	^ self readByteArray asString
]

{ #category : #reading }
HEBinaryReaderWriter >> readByteSymbol [
	^ self readByteString asSymbol 
]

{ #category : #reading }
HEBinaryReaderWriter >> readInt32 [
	| byteArray n |
	byteArray := stream next: self intSize.
	n := byteArray at: 4.
	n := (n bitShift: 8) + (byteArray at: 3).
	n := (n bitShift: 8) + (byteArray at: 2).
	n := (n bitShift: 8) + (byteArray at: 1).
	(byteArray at: 4) >= 128
		ifTrue: [ n := -16r100000000 + n ].
	^ n
]

{ #category : #reading }
HEBinaryReaderWriter >> readUInt32 [
	| byteArray n |
	byteArray := stream next: self intSize.
	n := byteArray at: 4.
	n := (n bitShift: 8) + (byteArray at: 3).
	n := (n bitShift: 8) + (byteArray at: 2).
	n := (n bitShift: 8) + (byteArray at: 1).
	^ n
]

{ #category : #accessing }
HEBinaryReaderWriter >> stream [
	^ stream
]

{ #category : #accessing }
HEBinaryReaderWriter >> stream: anObject [
	stream := anObject
]

{ #category : #writting }
HEBinaryReaderWriter >> writeByte: aNumber [
	stream nextPut: aNumber
]

{ #category : #writting }
HEBinaryReaderWriter >> writeByteArray: byteArray [
	self writeInt32: byteArray size.
	stream nextPutAll: byteArray.
]

{ #category : #writting }
HEBinaryReaderWriter >> writeByteString: aString [
	self writeByteArray: aString asByteArray
]

{ #category : #writting }
HEBinaryReaderWriter >> writeInt32: aNumber [
	| numberToSave |
	numberToSave := aNumber < 0
		ifTrue: [ (0 - aNumber) bitInvert32 + 1 ]
		ifFalse: [ aNumber ].
	1 to: 4 do: [ :i | stream nextPut: (numberToSave digitAt: i) ]
]

{ #category : #writting }
HEBinaryReaderWriter >> writeUInt32: aNumber [
	1 to: 4 do: [ :i | stream nextPut: (aNumber digitAt: i) ]
]
