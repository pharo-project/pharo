Class {
	#name : 'RBTransformationDummyRuleTest1',
	#superclass : 'RBFooDummyLintRuleTest1',
	#category : 'Refactoring-DataForTesting-LittleHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'LittleHierarchy'
}

{ #category : 'accessing' }
RBTransformationDummyRuleTest1 >> openEditor [
	| rules |
	rules := self failedRules.
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]
]
