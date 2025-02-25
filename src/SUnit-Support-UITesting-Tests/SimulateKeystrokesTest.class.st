"
SUnit tests to simulate and test key strokes
"
Class {
	#name : 'SimulateKeystrokesTest',
	#superclass : 'AbstractUITestCase',
	#category : 'SUnit-Support-UITesting-Tests-Base',
	#package : 'SUnit-Support-UITesting-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
SimulateKeystrokesTest >> testSimulateCmdKeystroke [

	| ws |
	ws := (Smalltalk tools toolNamed: #workspace) open.
	self simulateKeyStrokes: 'var := 3.' inWorld: ws world.
	self simulateKeyStroke: $s meta inWorld: ws world.
	self assert: ws hasUnacceptedEdits equals:false.
	ws delete
]

{ #category : 'tests' }
SimulateKeystrokesTest >> testSimulateKeystroke [

	| textMorph |
	textMorph := TextMorph new contents: ''; openInWorld.
	self assert: textMorph canChangeText.
	textMorph takeKeyboardFocus.
	self simulateKeyStroke: $s inWorld: textMorph world.
	self assert: textMorph contents asString equals: 's'.
	textMorph delete
]

{ #category : 'tests' }
SimulateKeystrokesTest >> testSimulateKeystrokes [

	| textMorph |
	textMorph := TextMorph new contents: ''; openInWorld.
	self assert: textMorph canChangeText.
	textMorph takeKeyboardFocus.
	self simulateKeyStrokes: 'hello' inWorld: textMorph world.
	self assert: textMorph contents asString equals: 'hello'.
	textMorph delete
]
