"
I represent composition of queries. 
I am abstract class and my subclasses implement concrete logic what to do with subqueries.
They should only implement three methods: 

- buildResult: aQueryResult 
Subclasses should deside what to do with result of subqueries.

- #unionWith: typedQueries as: aQueryResult
Subclasses should implement how union itself with given query collection.

- #, anotherQuery 
Subclasses should implement union with another query.

Other methods from the superclass I implement using delegation to my subqueries.

My instances can be created using #with: message: 

	ClyCompositeQuery with: { aQuery1. aQuery2 }
	ClyCompositeQuery with: { aQuery1. aQuery2 } as: aQueryResult
	
My scope is composition of scopes from all my subqueries. In general it is ClyCompositeScope instance. But in case of similar subscopes it can be single typed scope.

I redefine #description to print subqueries splitted by comma by default.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	subqueries:		<Set of: <ClyQuery>>
"
Class {
	#name : #ClyCompositeQuery,
	#superclass : #ClyQuery,
	#instVars : [
		'subqueries'
	],
	#category : #'Calypso-NavigationModel-Query'
}

{ #category : #'instance creation' }
ClyCompositeQuery class >> with: subqueries [
	subqueries ifEmpty: [ ^ClyUnknownQuery instance ].
	
	^self new 
		subqueries: subqueries
]

{ #category : #'instance creation' }
ClyCompositeQuery class >> with: subqueries as: aQueryResult [
	^(self with: subqueries)
		requiredResult: aQueryResult 
]

{ #category : #comparing }
ClyCompositeQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ subqueries = anObject subqueries
]

{ #category : #execution }
ClyCompositeQuery >> checkEmptyResult [
	^ subqueries allSatisfy: [ :each | each hasEmptyResult ]
]

{ #category : #execution }
ClyCompositeQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	subqueries do: [ :each | each collectMetadataOf: aQueryResult by: anEnvironmentPlugin ]
]

{ #category : #'item group decoration' }
ClyCompositeQuery >> decorateItemGroup: groupItem [
	subqueries do: [ :each | each decorateItemGroup: groupItem ]
]

{ #category : #printing }
ClyCompositeQuery >> description [
	^String streamContents: [ :s | self printExtraInfoOn: s]
]

{ #category : #testing }
ClyCompositeQuery >> executesQuery: aTypedQueryClass [
	^subqueries anySatisfy: [:each | each executesQuery: aTypedQueryClass ]
]

{ #category : #execution }
ClyCompositeQuery >> fixStateBeforeExecution [
	super fixStateBeforeExecution.
	
	subqueries do: [ :each | each fixStateBeforeExecution ]
]

{ #category : #comparing }
ClyCompositeQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: subqueries hash
]

{ #category : #'system changes' }
ClyCompositeQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^subqueries anySatisfy: [ :each | 
			each isResult: aQueryResult affectedBy: aSystemAnnouncement ]
]

{ #category : #printing }
ClyCompositeQuery >> printExtraInfoOn: aStream [
	| descriptions |
	super printExtraInfoOn: aStream.

	descriptions := subqueries collect: [:each | each description] as: Set.
	descriptions do: [:each | 
		aStream nextPutAll: each; nextPutAll: ', '].
	aStream skip: -2 "composite can not be empty"
]

{ #category : #converting }
ClyCompositeQuery >> restrictedByScope: aScope [
	| newSubqueries |
	newSubqueries := subqueries collect: [ :each | each restrictedByScope: aScope ].
	^self withSubqueries: newSubqueries
]

{ #category : #testing }
ClyCompositeQuery >> retrievesItem: anObject [
	^subqueries anySatisfy: [ :each | each retrievesItem: anObject ]
]

{ #category : #testing }
ClyCompositeQuery >> retrievesItemsOfType: itemTypeClass [
	
	^subqueries anySatisfy: [ :each | each retrievesItemsOfType: itemTypeClass ]
]

{ #category : #accessing }
ClyCompositeQuery >> subqueries [
	^ subqueries
]

{ #category : #accessing }
ClyCompositeQuery >> subqueries: aCollection [
	| subscopes |
	aCollection ifEmpty: [ ^self error: 'Composite query must have at least one subquery' ].
	
	subqueries := aCollection asSet.
	subscopes := subqueries collect: [:each | each scope].
	scope := ClyCompositeScope on: subscopes.
	self requiredResult: subqueries anyOne requiredResult 
]

{ #category : #converting }
ClyCompositeQuery >> withScope: aTypedScope [
	| newSubqueries |
	newSubqueries := subqueries collect: [ :each | each withScope: aTypedScope ].
	^self withSubqueries: newSubqueries 
]

{ #category : #converting }
ClyCompositeQuery >> withScopeOf: newBasisObjects [
	| newSubqueries |
	newSubqueries := subqueries collect: [ :each | each withScopeOf: newBasisObjects ].
	^self withSubqueries: newSubqueries
]

{ #category : #converting }
ClyCompositeQuery >> withSubqueries: newSubqueries [

	^self class with: newSubqueries as: requiredResult
]

{ #category : #converting }
ClyCompositeQuery >> withoutItemsOfType: anItemTypeClass [

	^self withSubqueries: (subqueries reject: [ :each | each retrievesItemsOfType: anItemTypeClass ])
]
