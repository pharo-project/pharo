Class {
	#name : 'ReTestCaseShouldNotUseInitializeRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'aDummyVar'
	],
	#category : 'General-Rules-Tests-Clean Code',
	#package : 'General-Rules-Tests',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReTestCaseShouldNotUseInitializeRuleTest >> aDummyVar [
	"Only necessary that ivar does not appear unused"

	^ aDummyVar
]

{ #category : 'running' }
ReTestCaseShouldNotUseInitializeRuleTest >> tearDown [

	self class removeSelector: #initialize.
	super tearDown
]

{ #category : 'tests' }
ReTestCaseShouldNotUseInitializeRuleTest >> testRule [

	self class
		compile: 'initialize
		super initialize.
		aDummyVar := 2 '
		classified: 'test-help'.

	self assert: (self myCritiquesOnClass: self class) size equals: 1
]

{ #category : 'tests' }
ReTestCaseShouldNotUseInitializeRuleTest >> testRuleDoesNotAppear [

	self class
		compile: 'initialize
		super initialize. '
		classified: 'test-help'.

	self assertEmpty: (self myCritiquesOnClass: self class)
]
