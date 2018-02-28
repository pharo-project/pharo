Class {
	#name : #SimulateKeystrokesSpecification,
	#superclass : #UITestCase,
	#category : #'SUnit-Support-UITesting-Tests'
}

{ #category : #tests }
SimulateKeystrokesSpecification >> testSimulateCmdKeystroke [

	| ws |
	ws := Smalltalk tools workspace open.
	self simulateKeyStrokes: 'var := 3.'.
	self simulateKeyStroke: $s meta.
	self assert: ws hasUnacceptedEdits = false.
	ws delete.
]

{ #category : #tests }
SimulateKeystrokesSpecification >> testSimulateKeystroke [

	| textMorph |
	textMorph := TextMorph new contents: ''; openInWorld.
	self assert: textMorph canChangeText.
	textMorph takeKeyboardFocus.
	self simulateKeyStroke: $s.
	self assert: textMorph contents asString equals: 's'.
	textMorph delete.
]

{ #category : #tests }
SimulateKeystrokesSpecification >> testSimulateKeystrokes [

	| textMorph |
	textMorph := TextMorph new contents: ''; openInWorld.
	self assert: textMorph canChangeText.
	textMorph takeKeyboardFocus.
	self simulateKeyStrokes: 'hello'.
	self assert: textMorph contents asString equals: 'hello'.
	textMorph delete.
]
