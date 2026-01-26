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
ReMethodsHaveNoSuperCallInSiblingsConditionTest >> testFailureWhenSiblingSendsSuperToTargetClass [

	| model target source cond |
	model := self modelOnClasses: { MyClassAlpha . MyClassBeta. MyClassBetaSub . MyClassBetaSubSub . MyClassBetaSibling }.
	target := model classNamed: #MyClassAlpha. 
	source := model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: source
		   targetSuperclass: target
		   selectors: { #methodForSupersend }.

	self deny: cond check
]

{ #category : 'tests' }
ReMethodsHaveNoSuperCallInSiblingsConditionTest >> testTargetClassIsSuperclassAndSuperSendInSubclass [

	| model target source cond |
	model := self modelOnClasses: { MyClassAlpha . MyClassBeta. MyClassBetaSub . MyClassBetaSubSub . MyClassBetaSibling }.
	target := model classNamed: #MyClassBeta. 
	source := model classNamed: #MyClassBetaSub.
	
	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: source
		   targetSuperclass: target
		   selectors: { #overridenInSubclassAndNoWhereElse }.

	self assert: cond check
]

{ #category : 'tests' }
ReMethodsHaveNoSuperCallInSiblingsConditionTest >> testTargetClassIsTwoLevelsAboveAndSuperSendInSubclass [

	| model target source cond |
	model := self modelOnClasses: { MyClassAlpha . MyClassBeta. MyClassBetaSub . MyClassBetaSubSub . MyClassBetaSibling }.
	target := model classNamed: #MyClassAlpha. 
	source := model classNamed: #MyClassBetaSub.
	
	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: source
		   targetSuperclass: target
		   selectors: { #overridenInSubclassAndNoWhereElse }.

	self assert: cond check
]
