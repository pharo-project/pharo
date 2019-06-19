"
I represent hierarchy of wrapper queries.
The wrapper query is a single item composite query which uses single subquery in own execution logic.

To create my instances use #for: message: 
	
	ClyWrapperQuery for: aQuery

Subclasses can use #actualQuery to access single subquery for convenience.

I redefine #description to look like subquery by default.
"
Class {
	#name : #ClyWrapperQuery,
	#superclass : #ClyCompositeQuery,
	#category : #'Calypso-NavigationModel-Query'
}

{ #category : #'instance creation' }
ClyWrapperQuery class >> for: aQuery [
	^self with: { aQuery }
]

{ #category : #accessing }
ClyWrapperQuery >> actualQuery [
	^subqueries anyOne
]

{ #category : #printing }
ClyWrapperQuery >> description [

	^self actualQuery description
]

{ #category : #printing }
ClyWrapperQuery >> printExtraInfoOn: aStream [

	^aStream nextPutAll: self description
]

{ #category : #accessing }
ClyWrapperQuery >> subqueries: queriesCollection [
	queriesCollection size = 1 ifFalse: [ self error: 'Wrapper query should include single subquery' ].
	super subqueries: queriesCollection
]

{ #category : #converting }
ClyWrapperQuery >> withResult: aQueryResult [
	
	| newQuery |
	newQuery := super withResult: aQueryResult.
	newQuery subqueries: (subqueries collect: [ :each | each withResult: aQueryResult ]).
	^newQuery
]

{ #category : #converting }
ClyWrapperQuery >> withSubqueries: newSubqueries [

	^self copy 
		subqueries: newSubqueries
]
