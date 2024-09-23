Class {
	#name : 'ReMethodsHaveNoDuplicatesTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesTest >> model [

	^ model ifNil: [
		  model := RBNamespace onEnvironment:
			           (RBClassEnvironment classes: {
					            MyClassBeta.
					            MyClassBetaSub.
					            MyClassAlpha }) ]
]

{ #category : 'tests' }
ReMethodsHaveNoDuplicatesTest >> testMethodDuplicatedInSibling [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		selectors: { #methodDuplicatedInSibling };
		class: myClassBeta;
		yourself.
		
	" the method has a duplicate in MyClassBeta's sibling (MyClassBetaSibling), therefore the condition fails "
	self deny: cond check
]

{ #category : 'tests' }
ReMethodsHaveNoDuplicatesTest >> testMethodNotDuplicatedInSibling [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		selectors: { #methodForPullUp };
		class: myClassBeta;
		yourself.
		
	" the method has no duplicate, therefore the condition succeeds "
	self assert: cond check
]

{ #category : 'tests' }
ReMethodsHaveNoDuplicatesTest >> testMethodNotDuplicatedInSubclass [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		selectors: { #methodDuplicatedInSubclass };
		class: myClassBeta;
		yourself.
		
	" the method has a duplicate in MyClassBeta's subclass, therefore the condition fails "
	self deny: cond check
]
