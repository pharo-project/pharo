"
SUnit tests for BlockClosure
"
Class {
	#name : 'BlockClosureTallyTest',
	#superclass : 'TestCase',
	#instVars : [
		'aBlock'
	],
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
BlockClosureTallyTest >> setUp [
	super setUp.
	"we reference self to force a full block"
	aBlock := [self . 100@100 corner: 200@200].
]

{ #category : 'tests' }
BlockClosureTallyTest >> testRunSimulated [
	self assert: (Context runSimulated: aBlock) class equals: Rectangle
]

{ #category : 'tests' }
BlockClosureTallyTest >> testTallyInstructions [

	| expected |
	expected := Smalltalk compiler compilationContext
		            optionCleanBlockClosure
		            ifTrue: [ 26 ]
		            ifFalse: [ 27 ].
	self
		assert: (Context tallyInstructions: aBlock) size
		equals: expected
]

{ #category : 'tests' }
BlockClosureTallyTest >> testTallyMethods [
	self assert: (Context tallyMethods: aBlock) size equals: 7
]
