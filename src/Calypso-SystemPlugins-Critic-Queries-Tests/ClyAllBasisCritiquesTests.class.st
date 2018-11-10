Class {
	#name : #ClyAllBasisCritiquesTests,
	#superclass : #ClyCritiqueQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyAllBasisCritiquesTests >> createQuery [
	^ClyAllBasisCritiqueQuery from: self scopeWithCritiques 
]

{ #category : #tests }
ClyAllBasisCritiquesTests >> testCheckIfEmpty [
		
	query scope: self scopeWithCritiques.
	self deny: query checkEmptyResult.
	
	query scope: self scopeWithoutCritiques.
	self assert: query checkEmptyResult.
]

{ #category : #tests }
ClyAllBasisCritiquesTests >> testExecution [
	
	self executeQuery.
	
	self assert: resultItems size equals: 1
]
