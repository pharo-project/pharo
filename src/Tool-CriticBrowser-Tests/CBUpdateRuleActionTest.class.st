Class {
	#name : #CBUpdateRuleActionTest,
	#superclass : #CBAbstractButtonActionTest,
	#category : #'Tool-CriticBrowser-Tests-Test-Button'
}

{ #category : #running }
CBUpdateRuleActionTest >> setUp [
	super setUp.
]

{ #category : #tests }
CBUpdateRuleActionTest >> testUpdateButtonHaveSameNumberOfToDo [
	| numberOfTodoBefore |
	self setupSelectionRulesPresenter: criticBrowser.
	numberOfTodoBefore := criticBrowser computeToSort: criticBrowser rule.
	CBUpdateRuleAction runOn: criticBrowser.
	self
		assert: (criticBrowser computeToSort: criticBrowser rule)
		equals: numberOfTodoBefore
]

{ #category : #tests }
CBUpdateRuleActionTest >> testUpdateWhenAddMethodAction [
	[ | beforeReset |
	self setupSelectRBUnclassifiedMethodRule.
	beforeReset := criticBrowser computeToSort: criticBrowser rule.
	UseForTestCriticBrowser
		compile:
			'toto
	 ^ ''klm'' , ''klm'''
		classified: 'as yet unclassified'.
	CBUpdateRuleAction runOn: criticBrowser.
	self
		assert: (criticBrowser computeToSort: criticBrowser rule)
		equals: beforeReset + 1 ]
		ensure: [ (UseForTestCriticBrowser >> #toto) removeFromSystem ]
]
