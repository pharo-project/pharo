Class {
	#name : #SendsDeprecatedMethodToGlobalRuleTest,
	#superclass : #TestCase,
	#instVars : [
		'testClass',
		'globalName'
	],
	#category : #'Kernel-Tests-Rules'
}

{ #category : #properties }
SendsDeprecatedMethodToGlobalRuleTest >> deprecatedMethodName [

	^ 'deprecatedMethod'
]

{ #category : #properties }
SendsDeprecatedMethodToGlobalRuleTest >> globalName [

	^ globalName ifNil: [ 
		[ globalName :=
			('TestGlobal', 1e10 atRandom asString) asSymbol
		] doWhileTrue: [ 
			Smalltalk globals includesKey: globalName ] ]
]

{ #category : #properties }
SendsDeprecatedMethodToGlobalRuleTest >> nonDeprecatedMethodName [

	^ 'not', self deprecatedMethodName 
]

{ #category : #running }
SendsDeprecatedMethodToGlobalRuleTest >> setUp [
	| deprClass |
	super setUp.
	deprClass := Object newAnonymousSubclass. 
	Smalltalk globals at: self globalName put: deprClass new.
	
	deprClass compile: self deprecatedMethodName, ' self deprecated: ''this method is deprecated'''.
	deprClass compile: self nonDeprecatedMethodName.
	
	testClass := Object newAnonymousSubclass.
	
]

{ #category : #running }
SendsDeprecatedMethodToGlobalRuleTest >> tearDown [

	Smalltalk globals removeKey: self globalName ifAbsent: [ ].
	globalName := nil.
	testClass := nil.
	super tearDown 
]

{ #category : #tests }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck [

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self deprecatedMethodName).

	self assert: (testMethod externalProperties anySatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]

{ #category : #tests }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck1 [

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self nonDeprecatedMethodName).

	self assert: (testMethod externalProperties noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]

{ #category : #tests }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck2 [

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ').

	self assert: (testMethod externalProperties noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]

{ #category : #tests }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck3 [

	| testMethod |
	
	testClass addClassVarNamed: #TestVar.
	
	testMethod := testClass >> (
		testClass compile: 'testMethod TestVar yourself').

	self assert: (testMethod externalProperties noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]
