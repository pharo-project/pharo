Class {
	#name : #OverridesDeprecatedMethodRuleTest,
	#superclass : #TestCase,
	#instVars : [
		'testClass',
		'testSubclass'
	],
	#category : 'Kernel-Tests-Rules'
}

{ #category : #properties }
OverridesDeprecatedMethodRuleTest >> methodName [

	^ 'testMethod'
]

{ #category : #running }
OverridesDeprecatedMethodRuleTest >> setUp [

	testClass := Object newAnonymousSubclass.
	testSubclass := testClass newAnonymousSubclass.
	
	testClass compile: self methodName, ' self deprecated: ''method is deprecated'''.
]

{ #category : #running }
OverridesDeprecatedMethodRuleTest >> tearDown [

	testClass := nil.
	testSubclass := nil
]

{ #category : #tests }
OverridesDeprecatedMethodRuleTest >> testBasicCheck [

	| testMethod |
	testMethod := testSubclass >> (
		testSubclass compile: self methodName).
		
	self assert: (OverridesDeprecatedMethodRule new basicCheck: testMethod)
]

{ #category : #tests }
OverridesDeprecatedMethodRuleTest >> testBasicCheck1 [

	| testMethod |
	testMethod := testSubclass >> (
		testSubclass compile: self methodName, '1').
		
	self deny: (OverridesDeprecatedMethodRule new basicCheck: testMethod)
]
