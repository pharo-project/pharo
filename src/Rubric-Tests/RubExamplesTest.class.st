Class {
	#name : 'RubExamplesTest',
	#superclass : 'TestCase',
	#category : 'Rubric-Tests-Examples',
	#package : 'Rubric-Tests',
	#tag : 'Examples'
}

{ #category : 'helpers' }
RubExamplesTest >> newWorld [
	"A world of the test's own, with a hand: windows ask the world's active hand for the keyboard focus when they open"

	| world hand |
	world := WorldMorph new.
	hand := HandMorph new.
	world addHand: hand.
	world activeHand: hand.
	^ world
]

{ #category : 'tests' }
RubExamplesTest >> testRubLipsumBasicExampleSmoke [

	| world |
	world := self newWorld.
	self shouldnt: [ RubLipsumBasicExample openNotWrappedIn: world ] raise: Error.
	self shouldnt: [ RubLipsumBasicExample openWrappedIn: world ] raise: Error
]

{ #category : 'tests' }
RubExamplesTest >> testRubLipsumWithSegmentsExampleSmoke [

	| world |
	world := self newWorld.
	self shouldnt: [ RubLipsumWithSegmentsExample openNotWrappedIn: world ] raise: Error.
	self shouldnt: [ RubLipsumWithSegmentsExample openWrappedIn: world ] raise: Error
]

{ #category : 'tests' }
RubExamplesTest >> testRubTextFieldExampleSmoke [

	self shouldnt: [ RubTextFieldExample openIn: self newWorld ] raise: Error
]

{ #category : 'tests' }
RubExamplesTest >> testRubTextFieldWithEntryCompletionExampleSmoke [

	self shouldnt: [ RubTextFieldWithEntryCompletionExample openIn: self newWorld ] raise: Error
]

{ #category : 'tests' }
RubExamplesTest >> testRubWorkspaceExampleSmoke [

	| world |
	world := self newWorld.
	self shouldnt: [ RubWorkspaceExample openIn: world ] raise: Error.
	self shouldnt: [ RubWorkspaceExample openWithCustomShortcutIn: world ] raise: Error
]
