Class {
	#name : #ClyCritiqueQueryTestCase,
	#superclass : #ClyTypedQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #testing }
ClyCritiqueQueryTestCase class >> isAbstract [
	^self = ClyCritiqueQueryTestCase
]

{ #category : #examples }
ClyCritiqueQueryTestCase >> methodWithCritiques [
	^ ClyClassWithProblemMethods >> #methodWithHalt
]

{ #category : #examples }
ClyCritiqueQueryTestCase >> methodWithoutCritiques [
	^ ClyClassWithProblemMethods >> #methodWithoutCritiques
]

{ #category : #examples }
ClyCritiqueQueryTestCase >> scopeWithCritiques [
	^ ClyMethodScope of: self methodWithCritiques in: environment
]

{ #category : #examples }
ClyCritiqueQueryTestCase >> scopeWithoutCritiques [
	^ ClyMethodScope of: self methodWithoutCritiques in: environment
]
