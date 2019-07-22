"
A CBFalsePositiveCriticActionTest is a test class for testing the behavior of CBFalsePositiveCriticAction
"
Class {
	#name : #CBFalsePositiveCriticActionTest,
	#superclass : #CBAbstractButtonActionTest,
	#category : #'Tool-CriticBrowser-Tests-Test-Button'
}

{ #category : #setUp }
CBFalsePositiveCriticActionTest >> instanceOfCriticBrowser [
	^ MockCriticBrowserFalsePositiveButton
]

{ #category : #running }
CBFalsePositiveCriticActionTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp
]

{ #category : #tests }
CBFalsePositiveCriticActionTest >> testCBFalsePositiveActionCallMarkAsFalsePositiveFunction [
	| mock |
	self setupSelectionRulesPresenter: criticBrowser.
	self setupSelectionCriticPresenter: criticBrowser.
	criticBrowser selectedCritic ifNil: [ self fail ].
	CBFalsePositiveCriticAction runOn: criticBrowser .
	self assert: criticBrowser messageCall equals: 1

]
