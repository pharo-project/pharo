"
Common superclass for Monticello readers
"
Class {
	#name : #MCReader,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : 'Monticello-Storing'
}

{ #category : #testing }
MCReader class >> canReadFileNamed: fileName [
	^ (fileName endsWith: self extension)
]

{ #category : #testing }
MCReader class >> concreteSubclasses [
	^ self allSubclasses reject: [:c | c isAbstract]
]

{ #category : #testing }
MCReader class >> isAbstract [
	^ (self respondsTo: #extension) not
]

{ #category : #'instance creation' }
MCReader class >> on: aStream [
	^ self new stream: aStream
]

{ #category : #'instance creation' }
MCReader class >> on: aStream name: aFileName [
	| class |
	class := self readerClassForFileNamed: aFileName.
	^ class
		ifNil: [self error: 'Unsupported format: ', aFileName]
		ifNotNil: [class on: aStream]
]

{ #category : #testing }
MCReader class >> readerClassForFileNamed: fileName [
	^ self concreteSubclasses
		detect: [:c | c canReadFileNamed: fileName]
		ifNone: [nil]
]

{ #category : #accessing }
MCReader >> stream [
	stream closed ifTrue: [ stream open ].
	^ stream
	
]

{ #category : #accessing }
MCReader >> stream: aStream [
	stream := aStream
]
