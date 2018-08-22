"
Check if a subclass of TestCase ends with 'Test'
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


	^ (aClass isKindOf: TestCase class) and: [(aClass name asString endsWith: 'Test') not].
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
	^ 'A test class (subclass of TestCase) should have it''s name ending with ''Test'''
]

{ #category : #running }
ReTestClassNameShouldEndWithTest >> severity [
	^ #warning
]
