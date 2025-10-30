Class {
	#name : 'ReClassesAreAbstractTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReClassesAreAbstractTest >> classToTest [
	^ ReClassesAreAbstractCondition
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstract [

	| abstract cond |
	abstract := self model classNamed: #Number.
	cond := self classToTest new classes: { abstract }.
	
	self assert: cond check.
	self assert: cond nonViolators equals: { abstract }.
	self assert: cond violators isEmpty	
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstractMessage [
	| abstract cond |
	abstract := self model classNamed: #Number.
	
	cond := self classToTest new 
		classes: { abstract }.
		
	self assert: cond errorString isEmpty
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstractWithNegation [

	| abstract cond |
	abstract := self model classNamed: #Number.
	cond := self classToTest new classes: { abstract }.
	
	self assert: cond check.
	self assert: cond nonViolators equals: { abstract }.
	self assert: cond violators isEmpty.
	
	cond := cond not.
	self deny: cond check.
	self assert: cond violators equals: { abstract }.
	self assert: cond nonViolators isEmpty.
	
	
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstractWithNegationMessage [

	| abstract cond |
	abstract := self model classNamed: #Number.
	cond := self classToTest new classes: { abstract }.
	
	self assert: cond check.
	self assert: cond errorString isEmpty.
		
	cond := cond not.
	self deny: cond check.
	self assert: 'Number is an abstract class.' equals: cond errorString
	
	
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsNotAbstract [

	| concrete cond |
	concrete := self model classNamed: #Object.
	cond := self classToTest new classes: { concrete }.

	self deny: cond check.
	self assert: cond violators equals: { concrete }.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsNotAbstractMessage [

	| concrete cond |
	concrete := self model classNamed: #Object.
	cond := self classToTest new classes: { concrete }.
	self assert: 'Object is not an abstract class.' equals: cond errorString
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsNotAbstractWithNegation [

	| concrete cond |
	concrete := self model classNamed: #Object.
	cond := self classToTest new classes: { concrete }.

	self deny: cond check.
	self assert: cond violators equals: { concrete }.
	self assert: cond nonViolators isEmpty.
	
	cond := cond not.
	self assert: cond check.
	self assert: cond nonViolators equals: { concrete }.
	self assert: cond violators isEmpty.
	
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsNotAbstractWithNegationMessage [

	| concrete cond |
	concrete := self model classNamed: #Object.
	cond := self classToTest new classes: { concrete }.

	self deny: cond check.
	self assert: 'Object is not an abstract class.' equals: cond errorString.

	cond := cond not.
	self assert: cond check.
	self assert: cond errorString isEmpty
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testOneAbstractOneConcrete [

	| abstract concrete cond |
	abstract := self model classNamed: #Number.
	concrete := self model classNamed: #Object.
	cond := self classToTest new classes: {
			        abstract.
			        concrete }.

	self deny: cond check.
	self assert: cond violators equals: { concrete }.
	self assert: cond nonViolators equals: { abstract }
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testOneAbstractOneConcreteMessage [

	| abstract concrete cond |
	abstract := self model classNamed: #Number.
	concrete := self model classNamed: #Object.
	cond := self classToTest new classes: {
			        abstract.
			        concrete }.

	self assert: ('Object is not an abstract class.' match: cond errorString)
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testOneAbstractOneConcreteWithNegation [

	| abstract concrete areAbstract areConcrete |
	abstract := self model classNamed: #Number.
	concrete := self model classNamed: #Object.
	areAbstract := self classToTest new classes: {
			        abstract.
			        concrete }.

	self deny: areAbstract check.
	self assert: areAbstract violators equals: { concrete }.
	self assert: areAbstract nonViolators equals: { abstract }.
	
	areConcrete := areAbstract not.
	self deny: areConcrete check.
	self assert: areConcrete violators equals: { abstract }.
	self assert: areConcrete nonViolators equals: { concrete }.
	
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testOneAbstractOneConcreteWithNegationMessage [

	| abstract concrete areAbstract areConcrete |
	abstract := self model classNamed: #Number.
	concrete := self model classNamed: #Object.
	areAbstract := self classToTest new classes: {
			        abstract.
			        concrete }.
	" Not all classes are abstract => cond fails "
	self deny: areAbstract check.
	self assert: areAbstract errorString equals: 'Object is not an abstract class.'.
	
	" Not all classes are abstract — therefore, the negated condition (NOT) also fails.
	  This proves that `cond not check` is not equivalent to `cond check not`,
	  except in the special case where the condition applies to a single candidate. "
	areConcrete := areAbstract not.
	self deny: areConcrete check.
	self assert: areConcrete errorString equals: 'Number is an abstract class.'	
]
