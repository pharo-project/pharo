Class {
	#name : #MicElementTest,
	#superclass : #TestCase,
	#instVars : [
		'parser',
		'instance'
	],
	#category : #'Microdown-Tests-Parser'
}

{ #category : #testing }
MicElementTest class >> isAbstract [ 

	^ self = MicElementTest
]

{ #category : #tests }
MicElementTest >> instance [

	^ instance ifNil: [ instance := self testedClass new ]
]

{ #category : #factory }
MicElementTest >> listBlockClass [
	^ MicListBlock
]

{ #category : #factory }
MicElementTest >> orderedListBlockClass [
	^ MicOrderedListBlock
]

{ #category : #factory }
MicElementTest >> parser [
	^ self parserClass new
]

{ #category : #factory }
MicElementTest >> parserClass [
	^ MicroDownParser
]

{ #category : #factory }
MicElementTest >> rootBlockClass [
	^ MicRootBlock
]

{ #category : #running }
MicElementTest >> setUp [

	super setUp.
	parser := self parser. 
]
