Class {
	#name : 'SendsDeprecatedMethodToGlobalRuleTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'testClass',
		'globalName'
	],
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'properties' }
SendsDeprecatedMethodToGlobalRuleTest >> deprecatedMethodName [

	^ 'deprecatedMethod'
]

{ #category : 'properties' }
SendsDeprecatedMethodToGlobalRuleTest >> globalName [

	^ globalName ifNil: [
		[ globalName :=
			('TestGlobal', 1e10 atRandom asString) asSymbol
		] doWhileTrue: [
			"if the name already exists, we generate another one"
			testingEnvironment includesKey: globalName ] ]
]

{ #category : 'properties' }
SendsDeprecatedMethodToGlobalRuleTest >> nonDeprecatedMethodName [

	^ 'not', self deprecatedMethodName
]

{ #category : 'running' }
SendsDeprecatedMethodToGlobalRuleTest >> setUp [
	| deprClass |
	super setUp.
	deprClass := Object newAnonymousSubclass.
	testingEnvironment at: self globalName put: deprClass new.

	deprClass compile: self deprecatedMethodName, ' self deprecated: ''this method is deprecated'''.
	deprClass compile: self nonDeprecatedMethodName.

	testClass := Object newAnonymousSubclass
]

{ #category : 'running' }
SendsDeprecatedMethodToGlobalRuleTest >> tearDown [

	testingEnvironment removeKey: self globalName ifAbsent: [ ].
	globalName := nil.
	testClass := nil.
	super tearDown
]

{ #category : 'tests' }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck [

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self deprecatedMethodName).

	self assert: (testMethod critiques anySatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]

{ #category : 'tests' }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck1 [

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ', self globalName, ' ', self nonDeprecatedMethodName).

	self assert: (testMethod critiques noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]

{ #category : 'tests' }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck2 [

	| testMethod |
	testMethod := testClass >> (
		testClass compile: 'testMethod ').

	self assert: (testMethod critiques noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]

{ #category : 'tests' }
SendsDeprecatedMethodToGlobalRuleTest >> testBasicCheck3 [

	| testMethod |

	testClass addClassVarNamed: #TestVar.

	testMethod := testClass >> (
		testClass compile: 'testMethod TestVar yourself').

	self assert: (testMethod critiques noneSatisfy: [ :eprop |
		eprop rule class = SendsDeprecatedMethodToGlobalRule ])
]
