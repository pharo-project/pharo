Class {
	#name : #CBUpdateRuleActionTest,
	#superclass : #CBAbstractButtonActionTest,
	#category : #'Tool-CriticBrowser-Tests-Tests'
}

{ #category : #tests }
CBUpdateRuleActionTest >> testUpdateButtonHaveSameNumberOfToDo [
	| numberOfTodoBefore |
	self selectInstanceVariableCapitalizationRule.
	numberOfTodoBefore := criticBrowser computeToSort: criticBrowser selectedRule.
	CBUpdateRuleAction runOn: criticBrowser.
	self assert: (criticBrowser computeToSort: criticBrowser selectedRule) equals: numberOfTodoBefore
]

{ #category : #tests }
CBUpdateRuleActionTest >> testUpdateWhenAddMethodAction [
	[ | beforeReset |
	self selectUnclassifiedMethodRule.
	beforeReset := criticBrowser computeToSort: criticBrowser selectedRule.
	UseForTestCriticBrowser
		compile:
			'toto
	 ^ ''klm'' , ''klm'''
		classified: 'as yet unclassified'.
	CBUpdateRuleAction runOn: criticBrowser.
	self assert: (criticBrowser computeToSort: criticBrowser selectedRule) equals: beforeReset + 1 ]
		ensure: [ (UseForTestCriticBrowser >> #toto) removeFromSystem ]
]
