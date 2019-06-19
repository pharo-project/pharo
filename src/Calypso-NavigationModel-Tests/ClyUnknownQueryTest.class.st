Class {
	#name : #ClyUnknownQueryTest,
	#superclass : #ClyQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyUnknownQueryTest >> createQuery [
	^ClyUnknownQuery new
]

{ #category : #running }
ClyUnknownQueryTest >> setUp [
	super setUp.
	"Unknown query is always bound to unknown scope which have no environment"
	environment := nil
]

{ #category : #tests }
ClyUnknownQueryTest >> testCheckIfEmpty [

	self assert: query hasEmptyResult.
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnknownQueryTest >> testCreationUnifiedInstance [

	self assert: query asUnifiedInstance == query
]

{ #category : #tests }
ClyUnknownQueryTest >> testDoesNotExecuteAnyQuery [
	
	self deny: (query executesQuery: #any)
]

{ #category : #tests }
ClyUnknownQueryTest >> testExecution [
	self executeQuery.
	
	self assert: result == ClyUnknownQueryResult instance.
	self assert: resultItems = #()
]

{ #category : #tests }
ClyUnknownQueryTest >> testGettingSingletonInstanceFromScratch [

	ClyUnknownQuery reset.
	
	self assert: ClyUnknownQuery instance == ClyUnknownQuery instance
]

{ #category : #tests }
ClyUnknownQueryTest >> testHasUnknownResultInstance [

	self assert: query requiredResult == ClyUnknownQueryResult instance
]

{ #category : #tests }
ClyUnknownQueryTest >> testIsBoundToEnvironmentWhenScopeIs [
	<expectedFailure>
	"Unknown query is never bound to the environment"
	super testIsBoundToEnvironmentWhenScopeIs
]
