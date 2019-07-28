"
SUnit tests to simulate and test key strokes
"
Class {
	#name : #SimulateKeystrokesTest,
	#superclass : #UITestCase,
	#category : #'SUnit-Support-UITesting-Tests'
}

{ #category : #tests }
SimulateKeystrokesTest >> testSimulateCmdKeystroke [
	| ws |
	ws := Smalltalk tools workspace open.
	ActiveHand simulateKeyStrokes: 'var := 3.'.
	ActiveHand simulateKeyStroke: $s meta.
	self assert: ws hasUnacceptedEdits equals: false.
	ws delete
]

{ #category : #tests }
SimulateKeystrokesTest >> testSimulateKeystroke [
	| textMorph |
	textMorph := TextMorph new
		contents: '';
		openInWorld.
	self assert: textMorph canChangeText.
	textMorph takeKeyboardFocus.
	ActiveHand simulateKeyStroke: $s.
	self assert: textMorph contents asString equals: 's'.
	textMorph delete
]

{ #category : #tests }
SimulateKeystrokesTest >> testSimulateKeystrokes [
	| textMorph |
	textMorph := TextMorph new
		contents: '';
		openInWorld.
	self assert: textMorph canChangeText.
	textMorph takeKeyboardFocus.
	ActiveHand simulateKeyStrokes: 'hello'.
	self assert: textMorph contents asString equals: 'hello'.
	textMorph delete
]
