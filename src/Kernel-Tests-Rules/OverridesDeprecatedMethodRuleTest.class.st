Class {
	#name : #OverridesDeprecatedMethodRuleTest,
	#superclass : #TestCase,
	#instVars : [
		'testClass',
		'testSubclass'
	],
	#category : #'Kernel-Tests-Rules'
}

{ #category : #properties }
OverridesDeprecatedMethodRuleTest >> methodName [

	^ 'testMethod'
]

{ #category : #running }
OverridesDeprecatedMethodRuleTest >> setUp [
	super setUp.
	testClass := Object newAnonymousSubclass.
	testSubclass := testClass newAnonymousSubclass.
	
	testClass compile: self methodName, ' self deprecated: ''method is deprecated'''.
]

{ #category : #running }
OverridesDeprecatedMethodRuleTest >> tearDown [

	testClass := nil.
	testSubclass := nil.
	super tearDown 
]

{ #category : #tests }
OverridesDeprecatedMethodRuleTest >> testBasicCheck [

	| testMethod |
	testMethod := testSubclass >> (
		testSubclass compile: self methodName).
		
	self assert: (OverridesDeprecatedMethodRule new check: testMethod) isNotEmpty 
]

{ #category : #tests }
OverridesDeprecatedMethodRuleTest >> testBasicCheck1 [

	| testMethod |
	testMethod := testSubclass >> (
		testSubclass compile: self methodName, '1').
		
	self assert: (OverridesDeprecatedMethodRule new check: testMethod) isEmpty
]
