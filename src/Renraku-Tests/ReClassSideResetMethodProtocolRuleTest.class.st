Class {
	#name : #ReClassSideResetMethodProtocolRuleTest,
	#superclass : #TestCase,
	#instVars : [
		'testClassName',
		'testClass'
	],
	#category : #'Renraku-Tests'
}

{ #category : #running }
ReClassSideResetMethodProtocolRuleTest >> setUp [

	super setUp.

	testClassName := #ReClassSideResetMethodProtocolRuleTestTestClass.
	testClass := Object subclass: testClassName
]

{ #category : #running }
ReClassSideResetMethodProtocolRuleTest >> tearDown [

	testClass removeFromSystem.

	super tearDown
]

{ #category : #running }
ReClassSideResetMethodProtocolRuleTest >> testAutomatedFix [

	| method critiques refactoring |
	testClass class
		compile: 'reset
	^ self'
		classified: 'class warfare'.

	method := testClass class methodNamed: #reset.

	critiques := ReClassSideResetMethodProtocolRule new check: method.
	refactoring := critiques first refactoring transform.
	RBRefactoryChangeManager instance performChange: refactoring changes.

	self assert: method protocol equals: #'class initialization'
]

{ #category : #running }
ReClassSideResetMethodProtocolRuleTest >> testIgnoredMethodWrongProtocol [

	| method critiques |
	testClass class compile: 'resetIrrelevant
	^ self' classified: 'class warfare'.

	method := (testClass class methodNamed: #resetIrrelevant).

	critiques := ReClassSideResetMethodProtocolRule new check: method.
	self assert: critiques isEmpty
]

{ #category : #running }
ReClassSideResetMethodProtocolRuleTest >> testRightMethodRightProtocol [

	| method critiques |
	testClass class compile: 'reset
	^ self' classified: 'class initialization'.

	method := (testClass class methodNamed: #reset).

	critiques := ReClassSideResetMethodProtocolRule new check: method.
	self assert: critiques isEmpty
]

{ #category : #running }
ReClassSideResetMethodProtocolRuleTest >> testRightMethodWrongProtocol [

	| method critiques |
	testClass class
		compile: 'reset
	^ self'
		classified: 'class warfare'.

	method := testClass class methodNamed: #reset.

	critiques := ReClassSideResetMethodProtocolRule
		             new check: method.
	self deny: critiques isEmpty
]
