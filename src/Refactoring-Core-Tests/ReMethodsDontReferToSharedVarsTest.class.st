Class {
	#name : 'ReMethodsDontReferToSharedVarsTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReMethodsDontReferToSharedVarsTest >> model [

	^ model ifNil: [ model := RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassBeta . MyClassBetaSub})]
]

{ #category : 'tests' }
ReMethodsDontReferToSharedVarsTest >> testMethodReferencingInstVarDefinedInItsOwnClassAndAnotherOneDefinedInItsSuperclass [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToLocalSharedVarsCondition new 
		class: myClassBeta selectors: { #methodReferencingSharedVarDefinedInItsDefiningClassAndOneInItsSuperclass }.
		
	" the method refers to and shared variable defined in its own defining class, therefore the condition fails "
	self deny: cond check.
	self assert: cond violators equals: { { ##methodReferencingSharedVarDefinedInItsDefiningClassAndOneInItsSuperclass . #SharedVarB } } asSet.
	self assert: cond nonViolators isEmpty.
]

{ #category : 'tests' }
ReMethodsDontReferToSharedVarsTest >> testMethodReferencingInstVarDefinedInItsOwnClassAndAnotherOneDefinedInItsSuperclassMessage [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToLocalSharedVarsCondition new 
		class: myClassBeta selectors: { #methodReferencingSharedVarDefinedInItsDefiningClassAndOneInItsSuperclass }.
		
	" the method refers to and shared variable defined in its own defining class, therefore the condition fails "
	self assert: ('*methodReferencingSharedVarDefinedInItsDefiningClassAndOneInItsSuperclass*SharedVarB*' match: cond errorString)
]

{ #category : 'tests' }
ReMethodsDontReferToSharedVarsTest >> testMethodReferencingNoSharedVariables [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToLocalSharedVarsCondition new 
		class: myClassBeta selectors: { #methodForPullUp }.
		
	" the method does not refer to shared variables "
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: {{ #methodForPullUp . nil }} asSet
]

{ #category : 'tests' }
ReMethodsDontReferToSharedVarsTest >> testMethodReferencingNoSharedVariablesMessage [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToLocalSharedVarsCondition new 
		class: myClassBeta selectors: { #methodForPullUp }.
		
	" the method does not refer to shared variables "
	self assert: cond errorString isEmpty
]

{ #category : 'tests' }
ReMethodsDontReferToSharedVarsTest >> testMethodRefersSharedVarDefinedInItsOwnClass [
	| myClassB cond |
	myClassB := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToLocalSharedVarsCondition new 
		class: myClassB selectors: { #methodReferencingSharedVariable }.
		
	" the method refers to a shared variable directly defined in MyClassBeta "
	self deny: cond check
]

{ #category : 'tests' }
ReMethodsDontReferToSharedVarsTest >> testMethodRefersSharedVarDefinedInItsSuperclass [
	| myClassB cond |
	myClassB := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToLocalSharedVarsCondition new 
		class: myClassB selectors: { #methodReferencingSharedVariableFromSuperclass }.
		
	" the method refers to a shared variable defined in MyClassBeta's superclass "
	self assert: cond check
]
