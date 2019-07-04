Class {
	#name : #ClyAllBasisCritiquesTest,
	#superclass : #ClyCritiqueQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyAllBasisCritiquesTest >> createQuery [
	^ClyAllBasisCritiqueQuery from: self scopeWithCritiques 
]

{ #category : #tests }
ClyAllBasisCritiquesTest >> testCheckIfEmpty [
		
	query scope: self scopeWithCritiques.
	self deny: query checkEmptyResult.
	
	query scope: self scopeWithoutCritiques.
	self assert: query checkEmptyResult.
]

{ #category : #tests }
ClyAllBasisCritiquesTest >> testExecution [
	
	self executeQuery.
	
	self assert: resultItems size equals: 1
]
