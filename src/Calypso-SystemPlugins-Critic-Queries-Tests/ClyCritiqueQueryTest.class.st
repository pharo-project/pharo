Class {
	#name : #ClyCritiqueQueryTest,
	#superclass : #ClyTypedQueryTest,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #testing }
ClyCritiqueQueryTest class >> isAbstract [
	^self = ClyCritiqueQueryTest
]

{ #category : #examples }
ClyCritiqueQueryTest >> methodWithCritiques [
	^ ClyClassWithProblemMethods >> #methodWithHalt
]

{ #category : #examples }
ClyCritiqueQueryTest >> methodWithoutCritiques [
	^ ClyClassWithProblemMethods >> #methodWithoutCritiques
]

{ #category : #examples }
ClyCritiqueQueryTest >> scopeWithCritiques [
	^ ClyMethodScope of: self methodWithCritiques in: environment
]

{ #category : #examples }
ClyCritiqueQueryTest >> scopeWithoutCritiques [
	^ ClyMethodScope of: self methodWithoutCritiques in: environment
]
