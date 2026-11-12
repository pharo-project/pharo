Class {
	#name : 'ZnUnpositionableReadStream',
	#superclass : 'Object',
	#instVars : [
		'stream'
	],
	#category : 'Zinc-Tests',
	#package : 'Zinc-Tests'
}

{ #category : 'instance creation' }
ZnUnpositionableReadStream class >> on: readStream [
	^ self new
		setStream: readStream;
		yourself
]

{ #category : 'testing' }
ZnUnpositionableReadStream >> atEnd [
	^ stream atEnd
]

{ #category : 'testing' }
ZnUnpositionableReadStream >> isBinary [
	^ true
]

{ #category : 'testing' }
ZnUnpositionableReadStream >> isStream [
	^ true
]

{ #category : 'accessing' }
ZnUnpositionableReadStream >> next [
	^ stream next
]

{ #category : 'accessing' }
ZnUnpositionableReadStream >> next: count [
	^ stream next: count
]

{ #category : 'accessing' }
ZnUnpositionableReadStream >> next: count into: collection startingAt: offset [
	^ stream next: count into: collection startingAt: offset
]

{ #category : 'accessing' }
ZnUnpositionableReadStream >> readInto: collection startingAt: offset count: count [
	^ stream readInto: collection startingAt: offset count: count
]

{ #category : 'initialization' }
ZnUnpositionableReadStream >> setStream: readStream [
	stream := readStream
]
