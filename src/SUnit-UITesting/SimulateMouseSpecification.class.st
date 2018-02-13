Class {
	#name : #SimulateMouseSpecification,
	#superclass : #UITestCase,
	#instVars : [
		'morph'
	],
	#category : #'SUnit-UITesting'
}

{ #category : #helpers }
SimulateMouseSpecification >> menuOpenedDuring: aBlock [

	| jailor |
	jailor := MenuCapturingMorph new openInWorld.
	aBlock value.
	^ jailor menu.
]

{ #category : #running }
SimulateMouseSpecification >> tearDown [

	morph delete.
	super tearDown 
]

{ #category : #tests }
SimulateMouseSpecification >> testSimulateClick [

	morph := TextMorph new contents: ''; openInWorld.	
	morph simulateClick.
	self assert: morph hasKeyboardFocus.
]

{ #category : #tests }
SimulateMouseSpecification >> testSimulateMiddleClick [
	
	morph := MorphHandlingMiddleButton new openInWorld.
	morph simulateMiddleClick.
	self assert: morph wasClickedWithMiddleButton.

]

{ #category : #tests }
SimulateMouseSpecification >> testSimulateRightClick [
	
	| menu |
	morph := Smalltalk tools workspace open.
	menu := self menuOpenedDuring: [ morph simulateRightClick ].
	self assert: menu isNil not.
]

{ #category : #helpers }
SimulateMouseSpecification >> timeoutAfter: aDuration ifNot: aBlock [

	| timer |
	timer := Stopwatch new activate.
	[ timer duration < aDuration and: [ aBlock value not ] ] whileTrue.
]

{ #category : #helpers }
SimulateMouseSpecification >> waitForMorphThat: aBlock [

	self timeoutAfter: 2 seconds ifNot: [
		morph := World submorphThat: aBlock ifNone: [ nil ].
		(Delay forMilliseconds: 100) wait.
		morph isNil].
	^ morph.
]
