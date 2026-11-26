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
ReMethodsHaveNoSuperCallInSiblingsConditionTest >> testMethodSuperSendingTwoPulledUpSelectorsIsReportedOncePerSelector [

	| model target source sibling cond |
	model := self modelOnClasses: { MyClassAlpha . MyClassBeta. MyClassBetaSub . MyClassBetaSubSub . MyClassBetaSibling }.
	target := model classNamed: #MyClassAlpha.
	source := model classNamed: #MyClassBeta.
	sibling := model classNamed: #MyClassBetaSibling.
	sibling removeMethod: #sendSuper.
	sibling compile: 'sendSuperTwice ^ super methodForSupersend + super methodOverriden' classified: #( #dummy ).

	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: source
		   targetSuperclass: target
		   selectors: { #methodForSupersend . #methodOverriden }.

	self
		assert: (cond violators collect: [ :each | each first ]) asSet
		equals: #( #methodForSupersend #methodOverriden ) asSet.
	self assert: cond violators size equals: 2
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

{ #category : 'tests' }
ReMethodsHaveNoSuperCallInSiblingsConditionTest >> testFailureWhenSuperSendExistsOnlyInModel [

	| model target source sibling cond |
	model := self modelOnClasses: { MyClassAlpha . MyClassBeta. MyClassBetaSub . MyClassBetaSubSub . MyClassBetaSibling }.
	target := model classNamed: #MyClassAlpha.
	source := model classNamed: #MyClassBeta.
	sibling := model classNamed: #MyClassBetaSibling.
	sibling removeMethod: #sendSuper.
	sibling compile: 'sendSuperFromModel ^ super methodForSupersend' classified: #( #dummy ).

	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: source
		   targetSuperclass: target
		   selectors: { #methodForSupersend }.

	self
		assert: (cond violators collect: [ :each | each third ]) asArray
		equals: #( #sendSuperFromModel )
]

{ #category : 'tests' }
ReMethodsHaveNoSuperCallInSiblingsConditionTest >> testViolatorsNameTheSiblingSuperSender [

	| model target source cond |
	model := self modelOnClasses: { MyClassAlpha . MyClassBeta. MyClassBetaSub . MyClassBetaSubSub . MyClassBetaSibling }.
	target := model classNamed: #MyClassAlpha.
	source := model classNamed: #MyClassBeta.

	cond := ReMethodsHaveNoSuperCallInSiblingsCondition new
			class: source
		   targetSuperclass: target
		   selectors: { #methodForSupersend }.

	self
		assert: cond violators asArray
		equals: { { #methodForSupersend . model classNamed: #MyClassBetaSibling . #sendSuper } }
]
