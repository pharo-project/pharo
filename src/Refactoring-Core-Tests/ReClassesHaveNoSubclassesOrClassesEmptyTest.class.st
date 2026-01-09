Class {
	#name : 'ReClassesHaveNoSubclassesOrClassesEmptyTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesHaveNoSubclassesOrClassesEmptyTest >> testClassHasNoSubclassesAndNotEmpty [

	| nosubs notempty hasNoSubs isEmpty noSubsOrEmpty noSubsAndEmpty hasNoSubsNot isEmptyNot noSubsOrEmptyNot noSubsAndEmptyNot |
	nosubs := self model classNamed: #MyClassB.
	notempty := self model classNamed: #MyClassAlpha.
	
	hasNoSubs := ReClassesHaveNoSubclassesCondition new
		        classes: { nosubs }.
	isEmpty := ReClassesEmptyCondition new 
					classes:  { notempty }.
	self assert: hasNoSubs check.
	self deny: isEmpty check.
	hasNoSubsNot := hasNoSubs not.
	isEmptyNot := isEmpty not.
	self deny: hasNoSubsNot check.
	self assert: isEmptyNot check.
	noSubsOrEmpty := hasNoSubs | isEmpty.
	self assert: noSubsOrEmpty check.
	noSubsAndEmpty:= hasNoSubs & isEmpty.
	self deny: noSubsAndEmpty check.
	noSubsOrEmptyNot := noSubsOrEmpty not.
	noSubsAndEmptyNot := noSubsAndEmpty not.
	self deny: noSubsOrEmptyNot check.
	self assert: noSubsAndEmptyNot check
		
]
