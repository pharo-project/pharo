Class {
	#name : #ClyConcreteGroupCritiquesTest,
	#superclass : #ClyFilteringCritiqueQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyConcreteGroupCritiquesTest >> createQuery [
	| exampleCritique |
	exampleCritique := self methodWithCritiques critiques anyOne.
	
	^ClyConcreteGroupCritiqueQuery 
		filter: ClyAllMethodCritiqueQuery new
		from: self scopeWithCritiques 
		byGroup: exampleCritique rule group
]

{ #category : #tests }
ClyConcreteGroupCritiquesTest >> testCheckIfEmpty [
	
	query scope: self scopeWithoutCritiques.
	self assert: query checkEmptyResult.

	query := self createQuery.		
	query scope: self scopeWithCritiques .
	self deny: query checkEmptyResult.
	
	query groupName: 'wrong group'.
	self assert: query checkEmptyResult.
]

{ #category : #tests }
ClyConcreteGroupCritiquesTest >> testComparisonWithQueryOfAnotherGroup [

	| query2 |
	query2 := self createQuery.
	query2 groupName: 'another group'.
	
	self deny: query = query2
]

{ #category : #tests }
ClyConcreteGroupCritiquesTest >> testExecution [

	self executeQuery.
	
	self assert: resultItems size equals: 1
]

{ #category : #tests }
ClyConcreteGroupCritiquesTest >> testExecutionWhenNoCritiquesFromRequiredGroup [

	query groupName: 'wrong group'.
	
	self executeQuery.
	
	self assert: resultItems isEmpty
]
