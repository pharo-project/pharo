Class {
	#name : 'ReClassesEmptyTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesEmptyTest >> testClassIsEmpty [

	| empty cond |
	empty := self model classNamed: #MyClassB.
	
	cond := ReClassesEmptyCondition new 
		classes: { empty }.
		
	self assert: cond check. 
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { empty }
	
]

{ #category : 'tests' }
ReClassesEmptyTest >> testClassIsEmptyWithMessage [

	| empty cond |
	empty := self model classNamed: #MyClassB.
	
	cond := ReClassesEmptyCondition new 
		classes: { empty }.
		
	self assert: cond errorString isEmpty
	
]

{ #category : 'tests' }
ReClassesEmptyTest >> testClassIsNotEmpty [

	| full cond |
	full := self model classNamed: #MyClassAlpha.
	
	cond := ReClassesEmptyCondition new 
		classes: { full }.
		
	self deny: cond check. 
	self assert: cond violators equals: { full }.
	self assert: cond nonViolators isEmpty
	
]

{ #category : 'tests' }
ReClassesEmptyTest >> testClassIsNotEmptyWithMessage [

	| full cond |
	full := self model classNamed: #MyClassAlpha.
	
	cond := ReClassesEmptyCondition new 
		classes: { full }.
		
	self assert: ('*' , full name , '*' match: cond errorString)
	
]

{ #category : 'tests' }
ReClassesEmptyTest >> testSomeClassesAreEmpty [

	| full empty cond |
	full := self model classNamed: #MyClassAlpha.
	empty := self model classNamed: #MyClassB.
	
	cond := ReClassesEmptyCondition new 
		classes: { full . empty }.
		
	self deny: cond check. 
	self assert: cond violators equals: { full }.
	self assert: cond nonViolators equals: { empty }
	
]

{ #category : 'tests' }
ReClassesEmptyTest >> testSomeClassesAreEmptyWithMessage [

	| full empty cond |
	full := self model classNamed: #MyClassAlpha.
	empty := self model classNamed: #MyClassB.
	
	cond := ReClassesEmptyCondition new 
		classes: { full . empty }.
		
	self assert: ('*' , full name , '*' match: cond errorString).
	self deny: ('*' , empty name , '*' match: cond errorString).
	
]
