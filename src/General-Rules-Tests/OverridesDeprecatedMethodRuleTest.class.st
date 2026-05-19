Class {
	#name : 'OverridesDeprecatedMethodRuleTest',
	#superclass : 'TestCase',
	#instVars : [
		'testClass',
		'testSubclass'
	],
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'properties' }
OverridesDeprecatedMethodRuleTest >> methodName [

	^ 'testMethod'
]

{ #category : 'running' }
OverridesDeprecatedMethodRuleTest >> setUp [
	super setUp.
	testClass := Object newAnonymousSubclass.
	testSubclass := testClass newAnonymousSubclass.

	testClass compile: self methodName, ' self deprecated: ''method is deprecated'''
]

{ #category : 'running' }
OverridesDeprecatedMethodRuleTest >> tearDown [

	testClass := nil.
	testSubclass := nil.
	super tearDown
]

{ #category : 'tests' }
OverridesDeprecatedMethodRuleTest >> testBasicCheck [

	| testMethod |
	testMethod := testSubclass >> (testSubclass compile: self methodName).
	self assert: testMethod overriddenMethods isNotEmpty.
	"the method overrides a method of the superclass"
	
	self assert: (OverridesDeprecatedMethodRule new check: testMethod) isNotEmpty.
	
	
]

{ #category : 'tests' }
OverridesDeprecatedMethodRuleTest >> testBasicCheck1 [
	| testMethod |
	testMethod := testSubclass >> (testSubclass compile: self methodName , '1').
	self assert: testMethod overriddenMethods isEmpty description: 'No overridden methods'.
	"the method does not override methods of the superclass"
	self assertEmpty: (OverridesDeprecatedMethodRule new check: testMethod)
]

{ #category : 'tests' }
OverridesDeprecatedMethodRuleTest >> testDoesNotActivateForDeprecatedMethods [
	| testMethod |
	testMethod := testSubclass >> (testSubclass compile: self methodName , ' self deprecated: ''Add another explanation message''').

	self assertEmpty: (OverridesDeprecatedMethodRule new check: testMethod)
]

{ #category : 'tests' }
OverridesDeprecatedMethodRuleTest >> testSetUp [

	self assert: (testClass includesSelector: #testMethod).
	self assert: (testClass compiledMethodAt: #testMethod) isDeprecated.
	self 
		assert: testSubclass superclass 
		identicalTo: testClass.
	
]
