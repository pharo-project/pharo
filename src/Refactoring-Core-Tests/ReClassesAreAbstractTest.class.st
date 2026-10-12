Class {
	#name : 'ReClassesAreAbstractTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReClassesAreAbstractTest >> model [

	^ model ifNil: [
		  model := RBNamespace onEnvironment: (RBClassEnvironment classes: { Object }).
		  model
			  defineClass: [ :aBuilder |
				  aBuilder
					  superclass: Object;
					  name: #ReClassesAreAbstractConcrete;
					  package: #'Refactory-Test data' ];
			  defineClass: [ :aBuilder |
				  aBuilder
					  superclass: Object;
					  name: #ReClassesAreAbstractReference;
					  package: #'Refactory-Test data' ];
			  defineClass: [ :aBuilder |
				  aBuilder
					  superclass: Object;
					  name: #ReClassesAreAbstractAbstract;
					  package: #'Refactory-Test data' ].
		  (model classNamed: #ReClassesAreAbstractReference)
			  compile: 'reference ^ ReClassesAreAbstractConcrete'
			  classified: #testing.
		  (model classNamed: #ReClassesAreAbstractAbstract)
			  compile: 'required self subclassResponsibility'
			  classified: #testing.
		  model ]
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstract [
	| abstractClass cond |
	abstractClass := self model classNamed: #ReClassesAreAbstractAbstract.
	
	cond := ReClassesAreAbstractCondition new 
		classes: { abstractClass }.
		
	"The fixture explicitly defines an abstract method."
	self assert: cond check.
	self assert: cond violators isEmpty.
	self assert: cond nonViolators equals: { abstractClass }
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testClassIsAbstractWithMessage [
	| abstractClass cond |
	abstractClass := self model classNamed: #ReClassesAreAbstractAbstract.
	
	cond := ReClassesAreAbstractCondition new 
		classes: { abstractClass }.
		
	self assert: cond errorString isEmpty
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testNoClassIsAbstract [

	| concrete cond |
	concrete := self model classNamed: #ReClassesAreAbstractConcrete.
	cond := ReClassesAreAbstractCondition new classes: { concrete }.

	self deny: cond check.
	self assert: cond violators equals: { concrete }.
	self assert: cond nonViolators isEmpty
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testNoClassIsAbstractWithMessage [

	| concrete cond |
	concrete := self model classNamed: #ReClassesAreAbstractConcrete.
	cond := ReClassesAreAbstractCondition new classes: { concrete }.

	self assert: ('*' , concrete name , '*' match: cond errorString)
]

{ #category : 'tests' }
ReClassesAreAbstractTest >> testSomeClassesAreAbstract [

	| abstract concrete cond |
	abstract := self model classNamed: #ReClassesAreAbstractAbstract.
	concrete := self model classNamed: #ReClassesAreAbstractConcrete.
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
	abstract := self model classNamed: #ReClassesAreAbstractAbstract.
	concrete := self model classNamed: #ReClassesAreAbstractConcrete.
	cond := ReClassesAreAbstractCondition new classes: {
			        abstract.
			        concrete }.

	self assert: ('*' , concrete name , '*' match: cond errorString).
	self deny: ('*' , abstract name , '*' match: cond errorString)
]
