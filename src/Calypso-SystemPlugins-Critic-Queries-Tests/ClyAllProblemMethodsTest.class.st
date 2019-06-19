Class {
	#name : #ClyAllProblemMethodsTest,
	#superclass : #ClyMethodQueryTestCase,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #running }
ClyAllProblemMethodsTest >> createQuery [
	^ClyAllProblemMethodQuery
		from: ClyClassScope of: ClyClassWithProblemMethods in: environment
]

{ #category : #tests }
ClyAllProblemMethodsTest >> testCheckIfEmpty [
	query scope: (ClyMethodScope
		of: (ClyClassWithProblemMethods >> #methodWithHalt) in: environment).
	self deny: query checkEmptyResult.
	
	query := self createQuery.
	query scope: (ClyMethodScope
		of: (ClyClassWithProblemMethods >> #methodWithoutCritiques) in: environment).
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyAllProblemMethodsTest >> testExecution [
	
	self executeQuery.
	
	self assert: foundSelectors equals: #(methodWithHalt methodWithHalt2)
]

{ #category : #tests }
ClyAllProblemMethodsTest >> testNotSelectsMethodWithCritiquesWhenTheyNotComputedYet [

	self deny: (query selectsMethod: (ClyClassWithProblemMethods >> #methodWithHalt))
]

{ #category : #tests }
ClyAllProblemMethodsTest >> testNotSelectsMethodWithoutCritiques [

	self deny: (query selectsMethod: (ClyClassWithProblemMethods >> #methodWithoutCritiques))
]

{ #category : #tests }
ClyAllProblemMethodsTest >> testSelectsMethodWithCritiquesWhenTheyExistAndComputedInAdvance [

	| critiques |
	critiques := query critiqueQuery async execute.
	[ critiques isBuilt ] whileFalse: [ 30 milliSeconds wait ].

	self assert: (query selectsMethod: (ClyClassWithProblemMethods >> #methodWithHalt))
]
