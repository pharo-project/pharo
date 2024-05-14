Class {
	#name : 'ReMethodsReferToSharedVariablesTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'tests' }
ReMethodsReferToSharedVariablesTest >> model [

	^ RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassBeta})
]

{ #category : 'tests' }
ReMethodsReferToSharedVariablesTest >> testMethodDoesNotReferToSharedVariables [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsReferToSharedVariablesCondition new 
		class: myClassBeta selectors: { #methodForPushUp }.
		
	" the method does not refer to shared variables "
	self deny: cond check
]

{ #category : 'tests' }
ReMethodsReferToSharedVariablesTest >> testMethodRefersSharedVariableDefinedInItsClass [
	| myClassB cond |
	myClassB := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsReferToSharedVariablesCondition new 
		class: myClassB selectors: { #methodReferencingSharedVariable }.
		
	" the method refers to a shared variable directly defined in MyClassBeta "
	self assert: cond check
]

{ #category : 'tests' }
ReMethodsReferToSharedVariablesTest >> testMethodRefersSharedVariableDefinedInItsSuperclass [
	| myClassB cond |
	myClassB := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsReferToSharedVariablesCondition new 
		class: myClassB selectors: { #methodReferencingSharedVariableFromSuperclass }.
		
	" the method refers to a shared variable defined in MyClassBeta's superclass "
	self deny: cond check
]
