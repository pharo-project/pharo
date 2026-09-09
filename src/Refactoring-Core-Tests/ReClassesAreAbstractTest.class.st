Class {
	#name : 'ReClassesAreAbstractTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstract [
	| classNumber cond |
	classNumber := self model classNamed: #Number.
	
	cond := ReClassesAreAbstractCondition new 
		classes: { classNumber }.
		
	" Number is abstract"
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { classNumber }
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstractWithMessage [
	| classNumber cond |
	classNumber := self model classNamed: #Number.
	
	cond := ReClassesAreAbstractCondition new 
		classes: { classNumber }.
		
	self assert: cond errorString isEmpty
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testNoClassIsAbstract [

	| concrete cond |
	concrete := self model classNamed: #Object.
	cond := ReClassesAreAbstractCondition new classes: { concrete }.

	self deny: cond check.
	self assert: cond violators equals: { concrete }.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testNoClassIsAbstractWithMessage [

	| concrete cond |
	concrete := self model classNamed: #Object.
	cond := ReClassesAreAbstractCondition new classes: { concrete }.

	self assert: ('*' , concrete name , '*' match: cond errorString)
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testSomeClassesAreAbstract [

	| abstract concrete cond |
	abstract := self model classNamed: #Number.
	concrete := self model classNamed: #Object.
	cond := ReClassesAreAbstractCondition new classes: {
			        abstract.
			        concrete }.

	self deny: cond check.
	self assert: cond violators equals: { concrete }.
	self assert: cond nonViolators equals: { abstract }
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testSomeClassesAreAbstractWithMessage [

	| abstract concrete cond |
	abstract := self model classNamed: #Number.
	concrete := self model classNamed: #Object.
	cond := ReClassesAreAbstractCondition new classes: {
			        abstract.
			        concrete }.

	self assert: ('*' , concrete name , '*' match: cond errorString).
	self deny: ('*' , abstract name , '*' match: cond errorString)
]
