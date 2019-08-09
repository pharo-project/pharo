"
Check if a subclass of TestCase ends with 'Test' or 'TestCase'.
"
Class {
	#name : #ReTestClassNameShouldEndWithTest,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReTestClassNameShouldEndWithTest class >> checksClass [

	^ true
]

{ #category : #running }
ReTestClassNameShouldEndWithTest >> basicCheck: aClass [
	| suffixes |
	suffixes := #('Test' 'TestCase').
	^ (aClass inheritsFrom: TestCase) and: [ suffixes noneSatisfy: [ :suffix | aClass name asString endsWith: suffix ] ]
]

{ #category : #running }
ReTestClassNameShouldEndWithTest >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReTestClassNameShouldEndWithTest >> name [
	^ 'A Test class'' name should end with ''Test'' '
]

{ #category : #accessing }
ReTestClassNameShouldEndWithTest >> rationale [
	^ 'A test class (subclass of TestCase) should have it''s name ending with ''Test'' or ''TestCase'''
]

{ #category : #running }
ReTestClassNameShouldEndWithTest >> severity [
	^ #warning
]
