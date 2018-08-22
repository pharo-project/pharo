"
Check if a test class' name ends with 'Tests' instead of 'Test'
"
Class {
	#name : #ReTestClassNameShouldNotEndWithTests,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReTestClassNameShouldNotEndWithTests class >> checksClass [

	^ true
]

{ #category : #running }
ReTestClassNameShouldNotEndWithTests >> basicCheck: aClass [

	^ (aClass isKindOf: TestCase class) and: [aClass name asString endsWith: 'Tests'].
]

{ #category : #running }
ReTestClassNameShouldNotEndWithTests >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReTestClassNameShouldNotEndWithTests >> name [
	^ 'A Test class'' name should not end with ''Tests'' '
]

{ #category : #accessing }
ReTestClassNameShouldNotEndWithTests >> rationale [
	^ 'A test class (subclass of TestCase) should have it''s name ending with ''Test'' instead of ''Tests'' '
]

{ #category : #running }
ReTestClassNameShouldNotEndWithTests >> severity [
	^ #warning
]
