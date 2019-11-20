Class {
	#name : #AbstractTwoButtonsTest,
	#superclass : #SpecTestCase,
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
	testedInstance firstAction: [ action1 := true ] secondAction: [ action2 := true ].
	testedInstance firstButton performAction.
	testedInstance secondButton performAction.
	self assert: action1.
	self assert: action2
]

{ #category : #tests }
AbstractTwoButtonsTest >> testFirstIconSecondIcon [
	testedInstance firstIcon: #thumbsUp secondIcon: #thumbsDown.
	self assert: testedInstance firstButton icon identicalTo: (self iconNamed: #thumbsUp).
	self assert: testedInstance secondButton icon identicalTo: (self iconNamed: #thumbsDown)
]

{ #category : #tests }
AbstractTwoButtonsTest >> testFirstLabelSecondLabel [
	testedInstance firstLabel: 'label 1' secondLabel: 'label 2'.
	self assert: testedInstance firstButton label asString equals: 'label 1'.
	self assert: testedInstance secondButton label asString equals: 'label 2'
]
