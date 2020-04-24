"
Check if a subclass of TestCase ends with 'Test' or 'TestCase'.
"
Class {
	#name : #ReTestClassNameShouldEndWithTestRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReTestClassNameShouldEndWithTestRule class >> checksClass [

	^ true
]

{ #category : #running }
ReTestClassNameShouldEndWithTestRule >> basicCheck: aClass [
	| suffixes |
	suffixes := #('Test' 'TestCase').
	^ (aClass inheritsFrom: TestCase) and: [ suffixes noneSatisfy: [ :suffix | aClass name asString endsWith: suffix ] ]
]

{ #category : #running }
ReTestClassNameShouldEndWithTestRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReTestClassNameShouldEndWithTestRule >> name [
	^ 'A Test class'' name should end with ''Test'' '
]

{ #category : #accessing }
ReTestClassNameShouldEndWithTestRule >> rationale [
	^ 'A test class (subclass of TestCase) should have it''s name ending with ''Test'' or ''TestCase'''
]

{ #category : #running }
ReTestClassNameShouldEndWithTestRule >> severity [
	^ #warning
]
