Class {
	#name : #ClyUnknownQueryTests,
	#superclass : #ClyQueryTestCase,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #running }
ClyUnknownQueryTests >> createQuery [
	^ClyUnknownQuery new
]

{ #category : #running }
ClyUnknownQueryTests >> setUp [
	super setUp.
	"Unknown query is always bound to unknown scope which have no environment"
	environment := nil
]

{ #category : #tests }
ClyUnknownQueryTests >> testCheckIfEmpty [

	self assert: query hasEmptyResult.
	self assert: query checkEmptyResult
]

{ #category : #tests }
ClyUnknownQueryTests >> testCreationUnifiedInstance [

	self assert: query asUnifiedInstance == query
]

{ #category : #tests }
ClyUnknownQueryTests >> testDoesNotExecuteAnyQuery [
	
	self deny: (query executesQuery: #any)
]

{ #category : #tests }
ClyUnknownQueryTests >> testExecution [
	self executeQuery.
	
	self assert: result == ClyUnknownQueryResult instance.
	self assert: resultItems = #()
]

{ #category : #tests }
ClyUnknownQueryTests >> testGettingSingletonInstanceFromScratch [

	ClyUnknownQuery reset.
	
	self assert: ClyUnknownQuery instance == ClyUnknownQuery instance
]

{ #category : #tests }
ClyUnknownQueryTests >> testHasUnknownResultInstance [

	self assert: query requiredResult == ClyUnknownQueryResult instance
]

{ #category : #tests }
ClyUnknownQueryTests >> testIsBoundToEnvironmentWhenScopeIs [
	<expectedFailure>
	"Unknown query is never bound to the environment"
	super testIsBoundToEnvironmentWhenScopeIs
]
