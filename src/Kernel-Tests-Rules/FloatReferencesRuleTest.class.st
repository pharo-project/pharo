Class {
	#name : #FloatReferencesRuleTest,
	#superclass : #TestCase,
	#instVars : [
		'testClass'
	],
	#category : #Kernel-Tests-Rules
}

{ #category : #running }
FloatReferencesRuleTest >> setUp [
	super setUp.
	testClass := Object newAnonymousSubclass.
]

{ #category : #running }
FloatReferencesRuleTest >> tearDown [

	testClass := nil.
	super tearDown 
]

{ #category : #tests }
FloatReferencesRuleTest >> testBasicCheck [

	| testMethod |
	testMethod := testClass >> (testClass compile: 'testMethod BoxedFloat64').
	
	self assert: (FloatReferencesRule new basicCheck: testMethod ast)
	 
]

{ #category : #tests }
FloatReferencesRuleTest >> testBasicCheck1 [

	| testMethod |
	testMethod := testClass >> (testClass compile: 'testMethod SmallFloat64').  
	
	self assert: (FloatReferencesRule new basicCheck: testMethod ast)
	 
]

{ #category : #tests }
FloatReferencesRuleTest >> testBasicCheck2 [

	| testMethod |
	testMethod := testClass >> (testClass compile: 'testMethod').  
	
	self deny: (FloatReferencesRule new basicCheck: testMethod ast)
	 
]
