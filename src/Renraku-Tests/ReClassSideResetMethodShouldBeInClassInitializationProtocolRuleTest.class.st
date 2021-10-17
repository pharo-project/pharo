Class {
	#name : #ReClassSideResetMethodShouldBeInClassInitializationProtocolRuleTest,
	#superclass : #TestCase,
	#instVars : [
		'testClassName',
		'testClass'
	],
	#category : #'Renraku-Tests'
}

{ #category : #running }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRuleTest >> setUp [
	super setUp.
	
	testClassName := #ReClassSideResetMethodShouldBeInClassInitializationProtocolRuleTestTestClass.
	testClass := Object subclass: testClassName.
 
]

{ #category : #running }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRuleTest >> tearDown [
	super tearDown.
	
	testClass removeFromSystem.
 
]

{ #category : #running }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRuleTest >> testIgnoredMethodWrongProtocol [

	| method critiques |
	testClass class compile: 'resetIrrelevant
	^ self' classified: 'class warfare'.
	
	method := (testClass class methodNamed: #resetIrrelevant).
	
	critiques := ReClassSideResetMethodShouldBeInClassInitializationProtocolRule new check: method.
	self assert: critiques isEmpty.
]

{ #category : #running }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRuleTest >> testRightMethodRightProtocol [

	| method results |
	testClass class compile: 'reset
	^ self' classified: 'class initialization'.
	
	method := (testClass class methodNamed: #reset).
	
	results := ReClassSideResetMethodShouldBeInClassInitializationProtocolRule new check: method.
	self assert: results isEmpty
]

{ #category : #running }
ReClassSideResetMethodShouldBeInClassInitializationProtocolRuleTest >> testRightMethodWrongProtocol [

	| method results |
	testClass class compile: 'reset
	^ self' classified: 'class warfare'.
	
	method := (testClass class methodNamed: #reset).
	
	results := ReClassSideResetMethodShouldBeInClassInitializationProtocolRule new check: method.
	self assert: results isNotEmpty.
]
