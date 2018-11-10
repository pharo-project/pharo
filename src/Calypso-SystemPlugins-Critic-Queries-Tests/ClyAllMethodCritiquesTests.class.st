Class {
	#name : #ClyAllMethodCritiquesTests,
	#superclass : #ClyCritiqueQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyAllMethodCritiquesTests >> createQuery [
	^ClyAllMethodCritiqueQuery from: self scopeWithCritiques 
]

{ #category : #tests }
ClyAllMethodCritiquesTests >> testCheckIfEmpty [
		
	query scope: self scopeWithCritiques .
	self deny: query checkEmptyResult.
	
	query scope: self scopeWithoutCritiques.
	self assert: query checkEmptyResult.
]

{ #category : #tests }
ClyAllMethodCritiquesTests >> testExecution [

	self executeQuery.
	
	self assert: resultItems size equals: 1
]
