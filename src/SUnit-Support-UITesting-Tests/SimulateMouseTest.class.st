"
SUnit tests to simulate and test mouse behavior
"
Class {
	#name : 'SimulateMouseTest',
	#superclass : 'AbstractUITestCase',
	#instVars : [
		'morph'
	],
	#category : 'SUnit-Support-UITesting-Tests-Base',
	#package : 'SUnit-Support-UITesting-Tests',
	#tag : 'Base'
}

{ #category : 'helpers' }
SimulateMouseTest >> menuOpenedDuring: aBlock [

	| jailor |
	jailor := MenuCapturingMorph new openInWorld.
	aBlock value.
	^ jailor menu
]

{ #category : 'running' }
SimulateMouseTest >> tearDown [

	morph delete.
	super tearDown
]

{ #category : 'tests' }
SimulateMouseTest >> testSimulateClick [

	morph := TextMorph new contents: ''; openInWorld.
	morph simulateClick.
	self assert: morph hasKeyboardFocus
]

{ #category : 'tests' }
SimulateMouseTest >> testSimulateMiddleClick [

	morph := MorphHandlingMiddleButton new openInWorld.
	morph simulateMiddleClick.
	self assert: morph wasClickedWithMiddleButton
]

{ #category : 'tests' }
SimulateMouseTest >> testSimulateRightClick [

	| menu |
	morph := (Smalltalk tools toolNamed: #workspace) open.
	menu := self menuOpenedDuring: [ morph simulateRightClick ].
	self assert: menu isNil not
]

{ #category : 'helpers' }
SimulateMouseTest >> timeoutAfter: aDuration ifNot: aBlock [

	| timer |
	timer := Stopwatch new activate.
	[ timer duration < aDuration and: [ aBlock value not ] ] whileTrue
]

{ #category : 'helpers' }
SimulateMouseTest >> waitForMorphThat: aBlock [

	self timeoutAfter: 2 seconds ifNot: [
		morph := self currentWorld submorphThat: aBlock ifNone: [ nil ].
		(Delay forMilliseconds: 100) wait.
		morph isNil].
	^ morph
]
