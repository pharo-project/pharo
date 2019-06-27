"
I am a union of subqueries.
I execute all subqueries and merge result items into single result instance.

My subqueries must be a kind of ClyTypedQuery. And I implement union logic to ensure this invariant.
During instance creation I try unify all subqueries. And in some cases the result query can be single type query instead of my instance.
For example:

	ClyUnionQuery with: { 
		ClyMessageSenders of: #(do:) from: (ClyClassScope of: Object in: environment). 
		ClyMessageSenders of: #(do:) from: (ClyClassScope of: String in: environment). 
	} 

It will return single senders query: 

	ClyMessageSenders of: #(do:) from: (ClyClassScope ofAll: {Object. String} in: environment).

But in general case the result union query will be return with subset of given queries with merged scope.

General query concatination using comma message produced my instances. Previous example can be rewritten as: 

	(ClyMessageSenders of: #(do:) from: (ClyClassScope of: Object in: environment))
		, (ClyMessageSenders of: #(do:) from: (ClyClassScope of: String in: environment))


"
Class {
	#name : #ClyUnionQuery,
	#superclass : #ClyCompositeQuery,
	#category : #'Calypso-NavigationModel-Query'
}

{ #category : #'merging queries' }
ClyUnionQuery class >> merge: queries [
	| result queryGroups |
	queries size = 1 ifTrue: [ ^{queries anyOne} ].
	
	result := OrderedCollection new.
	queryGroups := queries groupedBy: [ :each | each class ].
	queryGroups keysAndValuesDo: [ :queryClass :groupedQueries | 
		result addAll: (queryClass mergeOwnInstances: groupedQueries)].
	
	^result
]

{ #category : #'merging queries' }
ClyUnionQuery class >> mergeOwnInstances: queries [
	
	| allSubqueries |
	allSubqueries := queries flatCollect: [ :each | each subqueries ].
	^self merge: allSubqueries
]

{ #category : #'instance creation' }
ClyUnionQuery class >> with: subqueries [
	| mergedQueries |
	subqueries ifEmpty: [ ^ClyUnknownQuery instance ].
	
	mergedQueries := self merge: subqueries.
	mergedQueries size = 1 ifTrue: [ ^mergedQueries anyOne copy].
	
	^(super with: mergedQueries)
		requiredResult: subqueries anyOne requiredResult
]

{ #category : #composition }
ClyUnionQuery >> , anotherQuery [
	^ anotherQuery unionWith: subqueries as: requiredResult
]

{ #category : #execution }
ClyUnionQuery >> buildResult: aQueryResult [
	
	| subResult allItems |
	allItems := subqueries flatCollect: [ :each | 
		subResult := each execute.
		subResult items].
	
	aQueryResult fillWith: allItems asIdentitySet
]

{ #category : #composition }
ClyUnionQuery >> unionWith: typedQueries as: aQueryResult [

	^ClyQuery unionFrom: (typedQueries , subqueries) as: aQueryResult
]
