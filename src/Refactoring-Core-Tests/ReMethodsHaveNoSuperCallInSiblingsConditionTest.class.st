"
A ReMethodsHaveNoSuperCallInSiblingsConditionTest is a test class for testing the behavior of ReMethodsHaveNoSuperCallInSiblingsCondition
"
Class {
	#name : 'ReMethodsHaveNoSuperCallInSiblingsConditionTest',
	#superclass : 'RBAbstractConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReMethodsHaveNoSuperCallInSiblingsConditionTest >> testTargetClassIsTwoLevelsAboveAndSuperSendInSubclass [

	| model myClassAlpha myClassBetaSub cond |
	model := self modelOnClasses: { MyClassAlpha . MyClassBeta. MyClassBetaSub . MyClassBetaSubSub }.
	myClassAlpha := model classNamed: #MyClassAlpha. 
	myClassBetaSub := model classNamed: #MyClassBetaSub.
	
	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: myClassBetaSub
		   targetSuperclass: myClassAlpha
		   selectors: { #overridenInSubclassAndNoWhereElse }.

	self assert: cond check
]
