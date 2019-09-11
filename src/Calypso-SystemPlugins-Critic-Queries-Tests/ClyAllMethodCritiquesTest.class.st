Class {
	#name : #ClyAllMethodCritiquesTest,
	#superclass : #ClyCritiqueQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyAllMethodCritiquesTest >> createQuery [
	^ClyAllMethodCritiqueQuery from: self scopeWithCritiques 
]

{ #category : #tests }
ClyAllMethodCritiquesTest >> testCheckIfEmpty [
		
	query scope: self scopeWithCritiques .
	self deny: query checkEmptyResult.
	
	query scope: self scopeWithoutCritiques.
	self assert: query checkEmptyResult.
]

{ #category : #tests }
ClyAllMethodCritiquesTest >> testExecution [

	self executeQuery.
	
	self assert: resultItems size equals: 1
]
