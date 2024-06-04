"
Check if a test class' name ends with 'Tests' instead of 'Test'
"
Class {
	#name : 'ReTestClassNameShouldNotEndWithTests',
	#superclass : 'ReAbstractRule',
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : 'SUnit-Rules-SUnit',
	#package : 'SUnit-Rules',
	#tag : 'SUnit'
}

{ #category : 'testing-interest' }
ReTestClassNameShouldNotEndWithTests class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReTestClassNameShouldNotEndWithTests class >> group [

	^ 'SUnit'
]

{ #category : 'accessing' }
ReTestClassNameShouldNotEndWithTests class >> rationale [
	^ 'A test class (subclass of TestCase) should have it''s name ending with ''Test'' instead of ''Tests'' '
]

{ #category : 'accessing' }
ReTestClassNameShouldNotEndWithTests class >> ruleName [
	^ 'A Test class'' name should not end with ''Tests'' '
]

{ #category : 'running' }
ReTestClassNameShouldNotEndWithTests >> basicCheck: aClass [

	^ (aClass inheritsFrom: TestCase) and: [aClass name asString endsWith: 'Tests']
]
