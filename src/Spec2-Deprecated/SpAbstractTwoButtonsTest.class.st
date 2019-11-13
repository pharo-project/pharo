Class {
	#name : #SpAbstractTwoButtonsTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #deprecation }
SpAbstractTwoButtonsTest class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #running }
SpAbstractTwoButtonsTest >> classToTest [
	^ SpTestingAbstractTwoButtons
]

{ #category : #tests }
SpAbstractTwoButtonsTest >> testFirstActionSecondAction [
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
SpAbstractTwoButtonsTest >> testFirstIconSecondIcon [
	presenter firstIcon: #thumbsUp secondIcon: #thumbsDown.
	self assert: presenter firstButton icon == (self iconNamed: #thumbsUp).
	self assert: presenter secondButton icon == (self iconNamed: #thumbsDown)
]

{ #category : #tests }
SpAbstractTwoButtonsTest >> testFirstLabelSecondLabel [
	presenter firstLabel: 'label 1' secondLabel: 'label 2'.
	self assert: presenter firstButton label asString equals: 'label 1'.
	self assert: presenter secondButton label asString equals: 'label 2'
]
