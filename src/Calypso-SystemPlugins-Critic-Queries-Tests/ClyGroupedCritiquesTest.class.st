Class {
	#name : #ClyGroupedCritiquesTest,
	#superclass : #ClyQueryResultTestCase,
	#instVars : [
		'critic'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyGroupedCritiquesTest >> createQuery [

	^ClyAllProblemMethodQuery from: (ClyClassScope emptyIn: environment).	
]

{ #category : #running }
ClyGroupedCritiquesTest >> createQueryResult [
	^ClyGroupedCritiqueResult new
]

{ #category : #tests }
ClyGroupedCritiquesTest >> testFillingWithEmptyList [

	queryResult fillWith: { }.
	
	self assert: queryResult isEmpty
]

{ #category : #tests }
ClyGroupedCritiquesTest >> testFillingWithOneCritique [
	| method critique group |
	method := ClyClassWithProblemMethods >> #methodWithHalt.
	critique := method critiques anyOne.
	
	queryResult fillWith: { critique }.
	
	self assert: queryResult size equals: 1.
	group := queryResult items first.
	self assert: group class equals: ClyConcreteCritiqueMethodGroup.
	self assert: group name equals: critique rule group asLowercase.
	self assert: group ruleSeverity name equals: critique rule severity
]

{ #category : #tests }
ClyGroupedCritiquesTest >> testFillingWithTwoSimilarCritiques [
	| method method2 critique group |
	method := ClyClassWithProblemMethods >> #methodWithHalt.
	method2 := ClyClassWithProblemMethods >> #methodWithHalt2.
	critique := method critiques anyOne.
	
	queryResult fillWith: method critiques, method2  critiques.
	
	self assert: queryResult size equals: 1.
	group := queryResult items first.
	self assert: group class equals: ClyConcreteCritiqueMethodGroup.
	self assert: group name equals: critique rule group asLowercase.
	self assert: group ruleSeverity name equals: critique rule severity
]
