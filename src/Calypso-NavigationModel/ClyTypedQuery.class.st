"
My subclasses represent queries which retrieve particular kind of objects.
Subclasses should return class of items from class side method #resultItemsType.

I implement many required methods of superclass. 
The rest is responsibility of subclasses:

- buildResult: aQueryResult 
It is the method where query retrieves items from the scope and fill given result with them. Look at implementors.

- checkEmptyResult
Subclasses should be able detect that result will be empty without execution.

-isResult: aQueryResult affectedBy: aSystemAnnouncement
Any query can be affected by system changes. Subclasses should implement what change affects their results.

- retrivesItem: anObject
Subclasses should check that given item can be retrieved independently on scope.

-collectMetadataOf: aQueryResult by: anEnvironmentPlugin
Subclasses should dispatch metadata collection to the given environment plugin.

I provide many instance creation methods. For example you can execute any typed query with sorted result:

	ClyAllClasses sortedFrom: ClyNavigationEnvironment currentImageScope.

And I provide union query support. Typed queries can be concatinated using command message:
	aClassQuery, aMethodQuery

Look at class side for more options
"
Class {
	#name : #ClyTypedQuery,
	#superclass : #ClyQuery,
	#category : #'Calypso-NavigationModel-Query'
}

{ #category : #'instance creation' }
ClyTypedQuery class >> from: aScope [

	^aScope adoptQuery: self new
]

{ #category : #'instance creation' }
ClyTypedQuery class >> from: aScope as: aQueryResult [

	^(self from: aScope)
		requiredResult: aQueryResult
]

{ #category : #'instance creation' }
ClyTypedQuery class >> from: aScopeClass of: aBasisObject in: aNavigationEnvironment [
	^ self
		from: (aScopeClass of: aBasisObject in: aNavigationEnvironment)
]

{ #category : #'instance creation' }
ClyTypedQuery class >> from: aScopeClass ofAll: basisObjects in: aNavigationEnvironment [
	^ self
		from: (aScopeClass ofAll: basisObjects in: aNavigationEnvironment)
]

{ #category : #'instance creation' }
ClyTypedQuery class >> fromAll: scopeClasses of: aBasisObject in: aNavigationEnvironment [

	| subqueries |
	subqueries := scopeClasses collect: [:eachScope | 
		self from: eachScope of: aBasisObject in: aNavigationEnvironment].
	^ClyQuery unionFrom: subqueries
]

{ #category : #'merging queries' }
ClyTypedQuery class >> mergeOwnInstances: queries [

	| queryGroups merged |
	queryGroups := queries groupedBy: [ :each | each asUnifiedInstance ].
	
	merged := OrderedCollection new.
	queryGroups keysAndValuesDo: [ :unifiedQuery :groupedQueries |
		merged addAll: (self mergeOwnInstances: groupedQueries unifiedBy: unifiedQuery)].
	
	^merged
]

{ #category : #'merging queries' }
ClyTypedQuery class >> mergeOwnInstances: queries unifiedBy: ownUnifiedInstance [
	| scopeGroups result mergedBasis mergedScope |
	scopeGroups := queries groupedBy: [ :each | each scope asUnifiedInstance ].
	
	result := OrderedCollection new.
	scopeGroups keysAndValuesDo: [ :unifiedScope :groupedQueries | 
		mergedBasis := groupedQueries 
			flatCollect: [:each | each scope basisObjects] 
			as: OrderedCollection. "Merging duplications will be performed by scope itself"
		mergedScope := unifiedScope withNewBasisObjects: mergedBasis.
		result add: (ownUnifiedInstance withScope: mergedScope)].
		
	^result
]

{ #category : #'items type' }
ClyTypedQuery class >> resultItemsType [
	self subclassResponsibility 
]

{ #category : #'instance creation' }
ClyTypedQuery class >> sorted [
	^self sortedBy: self resultItemsType defaultSortFunctionForCalypso 
]

{ #category : #'instance creation' }
ClyTypedQuery class >> sortedFrom: aScope [
	
	^self sorted 
		scope: aScope
]

{ #category : #composition }
ClyTypedQuery >> , anotherQuery [
	^ anotherQuery unionWith: {self} as: requiredResult
]

{ #category : #private }
ClyTypedQuery >> asUnifiedInstance [
	^self copy 
		resetScope
]

{ #category : #'item group decoration' }
ClyTypedQuery >> decorateItemGroup: groupItem [
	groupItem markWithChildrenOf: self resultItemsType
]

{ #category : #testing }
ClyTypedQuery >> executesQuery: aTypedQueryClass [
	^self isKindOf: aTypedQueryClass 
]

{ #category : #converting }
ClyTypedQuery >> restrictedByScope: aScope [

	^self withScope: (scope restrictedBy: aScope)
]

{ #category : #accessing }
ClyTypedQuery >> resultItemsType [
	^self class resultItemsType
]

{ #category : #testing }
ClyTypedQuery >> retrievesItemsOfType: itemTypeClass [
	^ self resultItemsType isCalypsoItemType: itemTypeClass
]

{ #category : #composition }
ClyTypedQuery >> unionWith: typedQueries as: aQueryResult [

	^ClyQuery unionFrom: (typedQueries copyWith: self) as: aQueryResult 
]

{ #category : #converting }
ClyTypedQuery >> withScope: aScope [
	| copy |
	copy := self copy.
	^aScope adoptQuery: copy
]

{ #category : #converting }
ClyTypedQuery >> withScopeOf: newBasisObjects [

	^self withScope: (scope withNewBasisObjects: newBasisObjects)
]

{ #category : #converting }
ClyTypedQuery >> withoutItemsOfType: anItemTypeClass [

	^(self retrievesItemsOfType: anItemTypeClass)
		ifTrue: [ ClyUnknownQuery instance ]
		ifFalse: [ self ]
]
