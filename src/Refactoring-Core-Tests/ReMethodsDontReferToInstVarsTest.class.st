Class {
	#name : 'ReMethodsDontReferToInstVarsTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReMethodsDontReferToInstVarsTest >> model [

	^ model ifNil: [ model := RBNamespace onEnvironment:
		  (RBClassEnvironment classes: {MyClassBeta . MyClassBetaSub})]
]

{ #category : 'tests' }
ReMethodsDontReferToInstVarsTest >> testMethodReferencingInstVarDefinedInItsOwnClass [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToInstVarsCondition new 
		class: myClassBeta selectors: { #methodReferencingInstanceVariable }.
		
	" the method refers to an instance variable defined in its own defining class, therefore the condition fails "
	self deny: cond check.
	self assert: cond violators equals: { #(#methodReferencingInstanceVariable #instVarB) } asSet.
	self assert: cond nonViolators isEmpty 
]

{ #category : 'tests' }
ReMethodsDontReferToInstVarsTest >> testMethodReferencingInstVarDefinedInItsOwnClassAndAnotherOneDefinedInItsSuperclass [

	| myClassBetaSub cond |
	myClassBetaSub := self model classNamed: #MyClassBetaSub.

	cond := ReMethodsDontReferToInstVarsCondition new
		        class: myClassBetaSub
		        selectors: { #methodReferencingInstVarDefinedInItsDefiningClassAndOneInItsSuperclass }. " the method refers to and instance variable defined in its own defining class, therefore the condition fails "
	self deny: cond check.
	self assert: cond violators equals: { #( #methodReferencingInstVarDefinedInItsDefiningClassAndOneInItsSuperclass
		   #instVarBSub ) } asSet.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReMethodsDontReferToInstVarsTest >> testMethodReferencingInstVarDefinedInItsOwnClassWithMessage [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToInstVarsCondition new 
		class: myClassBeta selectors: { #methodReferencingInstanceVariable }.
		
	" the method refers to an instance variable defined in its own defining class, therefore the condition fails "
	self assert: ('*methodReferencingInstanceVariable*' match: cond errorString)
]

{ #category : 'tests' }
ReMethodsDontReferToInstVarsTest >> testMethodReferencingInstVarDefinedInSuperclass [
	| myClassBetaSub cond |
	myClassBetaSub := self model classNamed: #MyClassBetaSub.
	
	cond := ReMethodsDontReferToInstVarsCondition new 
		class: myClassBetaSub selectors: { #methodReferencingInstanceVariableDefinedInSuperclass }.
		
	" the method only refers to an instance variable which is not defined in its own defining class. Therefore the condition succeeds "
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { {#methodReferencingInstanceVariableDefinedInSuperclass . nil} } asSet
]

{ #category : 'tests' }
ReMethodsDontReferToInstVarsTest >> testMethodReferencingNoInstVars [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToInstVarsCondition new 
		class: myClassBeta selectors: { #methodForPullUp }.
		
	" the method does not refer to any instance variable therefore the condition succeeds "
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { {#methodForPullUp . nil} } asSet
]

{ #category : 'tests' }
ReMethodsDontReferToInstVarsTest >> testMethodReferencingNoInstVarsWithMessage [
	| myClassBeta cond |
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsDontReferToInstVarsCondition new 
		class: myClassBeta selectors: { #methodForPullUp }.
		
	" the method does not refer to any instance variable therefore the condition succeeds "
	self assert: cond errorString isEmpty
]
