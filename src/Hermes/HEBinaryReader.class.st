Class {
	#name : #HEBinaryReader,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #Hermes
}

{ #category : #'initialize-release' }
HEBinaryReader >> close [
	self stream close.
]

{ #category : #sizes }
HEBinaryReader >> intSize [
	^ 4
]

{ #category : #reading }
HEBinaryReader >> readByte [
	^ stream next
]

{ #category : #reading }
HEBinaryReader >> readByteArray [
	| byteArray size |
	size := self readInt32. 
	byteArray := stream next: size.
	^ byteArray
]

{ #category : #reading }
HEBinaryReader >> readByteString [
	^ self readByteArray asString
]

{ #category : #reading }
HEBinaryReader >> readByteSymbol [
	^ self readByteString asSymbol 
]

{ #category : #reading }
HEBinaryReader >> readInt32 [
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
HEBinaryReader >> readUInt32 [
	| byteArray n |
	byteArray := stream next: self intSize.
	n := byteArray at: 4.
	n := (n bitShift: 8) + (byteArray at: 3).
	n := (n bitShift: 8) + (byteArray at: 2).
	n := (n bitShift: 8) + (byteArray at: 1).
	^ n
]

{ #category : #accessing }
HEBinaryReader >> stream [
	^ stream
]

{ #category : #accessing }
HEBinaryReader >> stream: anObject [
	stream := anObject
]
