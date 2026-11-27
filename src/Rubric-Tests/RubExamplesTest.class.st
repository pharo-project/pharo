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
	RubLipsumBasicExample openNotWrappedIn: world.
	RubLipsumBasicExample openWrappedIn: world
]

{ #category : 'tests' }
RubExamplesTest >> testRubLipsumWithSegmentsExampleSmoke [

	| world |
	world := self newWorld.
	RubLipsumWithSegmentsExample openNotWrappedIn: world.
	RubLipsumWithSegmentsExample openWrappedIn: world
]

{ #category : 'tests' }
RubExamplesTest >> testRubTextFieldExampleSmoke [

	RubTextFieldExample openIn: self newWorld
]

{ #category : 'tests' }
RubExamplesTest >> testRubTextFieldWithEntryCompletionExampleSmoke [

	RubTextFieldWithEntryCompletionExample openIn: self newWorld
]

{ #category : 'tests' }
RubExamplesTest >> testRubWorkspaceExampleSmoke [

	| world |
	world := self newWorld.
	RubWorkspaceExample openIn: world.
	RubWorkspaceExample openWithCustomShortcutIn: world
]
