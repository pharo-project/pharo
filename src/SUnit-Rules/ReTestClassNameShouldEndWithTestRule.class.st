"
Check if a subclass of TestCase ends with 'Test' or 'TestCase'.
"
Class {
	#name : 'ReTestClassNameShouldEndWithTestRule',
	#superclass : 'ReAbstractRule',
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : 'SUnit-Rules-SUnit',
	#package : 'SUnit-Rules',
	#tag : 'SUnit'
}

{ #category : 'testing-interest' }
ReTestClassNameShouldEndWithTestRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReTestClassNameShouldEndWithTestRule class >> group [

	^ 'SUnit'
]

{ #category : 'accessing' }
ReTestClassNameShouldEndWithTestRule class >> rationale [
	^ 'A test class (subclass of TestCase) should have it''s name ending with ''Test'' or ''TestCase'''
]

{ #category : 'accessing' }
ReTestClassNameShouldEndWithTestRule class >> ruleName [
	^ 'A Test class'' name should end with ''Test'' '
]

{ #category : 'running' }
ReTestClassNameShouldEndWithTestRule >> basicCheck: aClass [
	| suffixes |
	suffixes := #('Test' 'TestCase').
	^ (aClass inheritsFrom: TestCase) and: [ suffixes noneSatisfy: [ :suffix | aClass name asString endsWith: suffix ] ]
]
