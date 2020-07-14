Class {
	#name : #MicBlockTest,
	#superclass : #TestCase,
	#instVars : [
		'parser'
	],
	#category : #'Microdown-Tests'
}

{ #category : #factory }
MicBlockTest >> listBlockClass [
	^ MicListBlock
]

{ #category : #factory }
MicBlockTest >> orderedListBlockClass [
	^ MicOrderedListBlock
]

{ #category : #factory }
MicBlockTest >> parser [
	^ self parserClass new
]

{ #category : #factory }
MicBlockTest >> parserClass [
	^ MicroDownParser
]

{ #category : #factory }
MicBlockTest >> rootBlockClass [
	^ MicRootBlock
]

{ #category : #running }
MicBlockTest >> setUp [

	super setUp.
	parser := self parser. 
]
