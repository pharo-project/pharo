"
An object you can teach how to behave. Have a look at the class side for an example.
The implementation was done by Torsten Bergman.

For more infos have a look at:
[https://github.com/astares/Pharo-Teachable](https://github.com/astares/Pharo-Teachable)

"
Class {
	#name : 'Teachable',
	#superclass : 'Object',
	#instVars : [
		'learnings'
	],
	#category : 'SUnit-MockObjects-Teachable',
	#package : 'SUnit-MockObjects',
	#tag : 'Teachable'
}

{ #category : 'examples' }
Teachable class >> example [

	| teachable |
	teachable := self new.
	teachable
		whenSend: #help return: 'ok';
		whenSend: #doit evaluate: [ 1 inspect ];
		acceptSend: #noDebugger;
		whenSend: #negate: evaluate: [ :num | num negated ].
	teachable help.
	teachable doit.
	teachable noDebugger.
	teachable negate: 120
]

{ #category : 'teaching' }
Teachable >> acceptSend: aSymbol [
	"Teach the receiver to accept a message with the given symbol"

	self whenSend: aSymbol return: self
]

{ #category : 'reflective operations' }
Teachable >> doesNotUnderstand: aMessage [
	"We override this message to check if the receiver was taught to understand the given message"

	| learning |
	learning := self learnings
		at: aMessage selector
		ifAbsent:[ ^super doesNotUnderstand: aMessage ].
	^ learning class == Association
		ifTrue: [learning value]
		ifFalse: [learning valueWithArguments: aMessage arguments]
]

{ #category : 'private' }
Teachable >> learnings [
	"Returns a dictionary with the receivers knowledge"

	learnings ifNil: [ learnings := Dictionary new ].
	^ learnings
]

{ #category : 'teaching' }
Teachable >> whenSend: aSymbol evaluate: aBlock [
	"Teach the receiver to evaluate a block when when he receives a message identified by the given Symbol"

	self learnings at: aSymbol put: aBlock
]

{ #category : 'teaching' }
Teachable >> whenSend: aSymbol return: anObject [
	"Teach the receiver to return the given object when he receives a message identified by the given Symbol"

	self learnings at: aSymbol put: (#return -> anObject)
]
