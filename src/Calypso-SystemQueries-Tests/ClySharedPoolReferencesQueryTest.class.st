Class {
	#name : 'ClySharedPoolReferencesQueryTest',
	#superclass : 'ClyClassQueryTest',
	#pools : [
		'ClySharedPoolForTesting'
	],
	#category : 'Calypso-SystemQueries-Tests-Queries',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Queries'
}

{ #category : 'running' }
ClySharedPoolReferencesQueryTest >> createQuery [

	^ ClySharedPoolReferencesQuery
		to: ClySharedPoolForTesting;
		from: (ClyClassScope of: Object in: environment)
]

{ #category : 'running' }
ClySharedPoolReferencesQueryTest >> setUp [
	super setUp.

	environment system: ClySystemEnvironment currentImage.
	environment detachFromSystem.

]

{ #category : 'tests' }
ClySharedPoolReferencesQueryTest >> testCheckIfEmpty [

	query variableQuery: (ClyConstantQuery returning: {(self class environment bindingOf: #ClySharedPoolForTesting)}).
	self queryFromScope: ClyClassScope of: self class.
	self deny: query checkEmptyResult
]

{ #category : 'tests' }
ClySharedPoolReferencesQueryTest >> testFromSystemScope [

	query variableQuery: (ClyConstantQuery returning: {(self class environment bindingOf: #ClySharedPoolForTesting)}).
	self queryFromScope: ClyClassScope of: self class.
	self assert: resultItems size equals: 1.
	self assertCollection: resultItems hasSameElements: { self class }
]
