Class {
	#name : #AbstractTwoButtonsTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-PolyWidgets-Support'
}

{ #category : #running }
AbstractTwoButtonsTest >> classToTest [
	^ TestingAbstractTwoButtons
]

{ #category : #tests }
AbstractTwoButtonsTest >> testFirstActionSecondAction [
	| action1 action2 |
	action1 := false.
	action2 := false.
	presenter firstAction: [ action1 := true ] secondAction: [ action2 := true ].
	presenter firstButton performAction.
	presenter secondButton performAction.
	self assert: action1.
	self assert: action2
]

{ #category : #tests }
AbstractTwoButtonsTest >> testFirstIconSecondIcon [
	presenter firstIcon: #thumbsUp secondIcon: #thumbsDown.
	self assert: presenter firstButton icon == (self iconNamed: #thumbsUp).
	self assert: presenter secondButton icon == (self iconNamed: #thumbsDown)
]

{ #category : #tests }
AbstractTwoButtonsTest >> testFirstLabelSecondLabel [
	presenter firstLabel: 'label 1' secondLabel: 'label 2'.
	self assert: presenter firstButton label asString equals: 'label 1'.
	self assert: presenter secondButton label asString equals: 'label 2'
]
