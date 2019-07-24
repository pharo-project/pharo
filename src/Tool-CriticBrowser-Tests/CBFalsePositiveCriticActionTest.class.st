"
A CBFalsePositiveCriticActionTest is a test class for testing the behavior of CBFalsePositiveCriticAction
"
Class {
	#name : #CBFalsePositiveCriticActionTest,
	#superclass : #CBAbstractButtonActionTest,
	#category : #'Tool-CriticBrowser-Tests-Test-Button'
}

{ #category : #tests }
CBFalsePositiveCriticActionTest >> testCBFalsePositiveActionCallMarkAsFalsePositiveFunction [
	self selectInstanceVariableCapitalizationRule.
	criticBrowser criticsPresenter selectIndex: 1.
	self deny: criticBrowser selectedCritic isNil.
	CBFalsePositiveCriticAction runOn: criticBrowser.
	self assert: criticBrowser isSelectedCriticFalsePositive
]
