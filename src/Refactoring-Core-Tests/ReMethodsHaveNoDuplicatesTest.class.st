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
					            MyClassAlpha.
									MyClassBetaSibling }) ]
]

{ #category : 'tests' }
ReMethodsHaveNoDuplicatesTest >> testMethodDuplicatedInSibling [

	| myClassAlpha myClassBetaSub cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBetaSub := self model classNamed: #MyClassBetaSub.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		selectors: { #methodDuplicatedInSibling };
		class: myClassBetaSub;
		yourself.
	
	"if we use myClassBeta we will not spot the duplication because the source (class) is always removed 
	to compute the influence zone (called so far siblings - but this is larger than siblings 
	it is more all the subclasses of target without the source."
	
	"the method has a duplicate in MyClassBeta's sibling (MyClassBetaSibling), therefore the condition fails "
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
		
	" the method has a duplicate in MyClassBeta's subclass, therefore the condition should fail"
	self deny: cond check
]

{ #category : 'tests' }
ReMethodsHaveNoDuplicatesTest >> testMethodNotDuplicatedInSubclassMoreComplex [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		selectors: { #duplicatedInSubclassMoreComplexBody };
		class: myClassBeta;
		yourself.
		
	" the method has a duplicate in MyClassBeta's subclass, therefore the condition should fail"
	self deny: cond check
]

{ #category : 'tests' }
ReMethodsHaveNoDuplicatesTest >> testMethodNotDuplicatedInSubclassWithDifferentSelector [
	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		selectors: { #methodDuplicatedInSubclass2 };
		class: myClassBeta;
		yourself.
		
	" the method has a different selector so it is not a duplicate in MyClassBeta's subclass, therefore the condition should pass"
	self assert: cond check
]

{ #category : 'tests - helpers' }
ReMethodsHaveNoDuplicatesTest >> testSiblingAreAlsoMother [
	"and this is really strange"
	
"
Alpha
   Beta
       BetaSub
   BetaSibling

It is unclear why beta would be a sibling of BetaSub. 
So may be sibling is not the correct name, 
A better name may be underInfluence e.g. all classes except the target and the source

"

	| myClassAlpha myClassBetaSub cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBetaSub := self model classNamed: #MyClassBetaSub.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		class: myClassBetaSub;
		yourself.
		
	self assert: cond siblingsOfSource size equals: 2.
	self assert: 
		(#(MyClassBetaSibling MyClassBeta) includesAll: 
		(cond siblingsOfSource collect: [ :each | each name ]))
]

{ #category : 'tests - helpers' }
ReMethodsHaveNoDuplicatesTest >> testSiblingIncludesSubclassesToo [


"
Alpha
   Beta
       BetaSub
   BetaSibling

"

	| myClassAlpha myClassBeta cond |
	myClassAlpha := self model classNamed: #MyClassAlpha. 
	myClassBeta := self model classNamed: #MyClassBeta.
	
	cond := ReMethodsHaveNoDuplicatesCondition new
		targetSuperclass: myClassAlpha;
		class: myClassBeta;
		yourself.
		
	self assert: cond siblingsOfSource size equals: 2.
	self assert: 
		(#(MyClassBetaSibling MyClassBetaSub) includesAll: 
		(cond siblingsOfSource collect: [ :each | each name ]))
]
