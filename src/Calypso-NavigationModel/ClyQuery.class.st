"
I am a root of hierarchy of queries.
My subclasses implement specific logic how to retrieve particular objects from given environment scope. 

Any query should be created with scope:

	query := ClyAllMethods from: scope
	
And to create scope instance you need some navigation environment. For example to query Smalltalk image there is global #currentImage environment: 

	scope := ClyClassScope of: Object in: ClyNavigationEnvironment currentImage.
	
When query instance is created you can simply execute it: 

	result := query execute.
	
The result of any query is a kind of ClyQueryResult.
By default it is always ClyRawQueryResult which do not apply any formatting or transformation on retrieved items.
The required result is a parameter of any query, the variable #requiredResult. Responsibility of result is to format or transform items retrieved by query. 
For example there are ClySortedQueryResult which sort items using given sort funciton.

The value of requiredResult variable is used as prototype to create actual result instances. During execution the query creates it using: 

	actualResult := requiredResult prepareNewFor: aQuery in: environment
	
You can specify requird result when you create query instances. For example: 

	ClyAllClassQuery from: packageScope as: ClySubclassHierarchy new asQueryResult.
	
My subclasses provide various constructors to specify such parameters.
	
Any query instance can be converted to query with new required result: 

	aQuery withResult: ClySpecialQueryResult new
	
There are other converting methods which are supported by any kind of queries: 

- withScope: aScope, it returnes similar query but with different scope
- withScopeOf: newBasisObjects, it returns similar query with scope of different basis
- restrictedByScope: aScope, it returnes similar query with similar scope restricted by basis of given scope.
- filtereBy: anItemFilter, it returns wrapper query which filters original query result using given filter

My subclasses must implement several methods: 
	
- buildResult: aQueryResult 
It is the method where query retrieves items from the scope and fill given result with them. Look at implementors.

- checkEmptyResult
It checks that result will be empty without full execution.

-isResult: aQueryResult affectedBy: aSystemAnnouncement
Any query can be affected by system changes. Subclasses should decide what changes can affect them.

- retrivesItem: anObject
Subclasses should check that given item can be retrieved. This check should not depends on query scope.

- retrivesItemOfType: aClass
Subclasses should check what kind of items they retrieve.

- executesQuery: aTypedQueryClass
Subclasses should check that they in fact executes given query class. For example composite query will ask subqueries for this question. But typed queries will use simple isKindOf: check.

- withScope: aScope 
Subclasses should implement converting to the similar query from new given scope.

- withScopeOf: newBasisObjects 
Subclasses should implement converting to the similar query from the similar scope of new given basis.
 
- #unionWith: typedQueries as: aQueryResult
Subclasses should implement converting to composite query union given collection of subqueries.

- #, anotherQuery 
Subclasses should implement union with another query.

-collectMetadataOf: aQueryResult by: anEnvironmentPlugin
Subclasses should dispatch metadata collection to the given environment plugin.

Also there is special ClyUnionQuery class which requires additional method to be implemented: 

- mergeOwnInstances: queries
It should merge similar query instances. Idea to reduce subqueries count in union query. Subclasses should produce single query with merged collection of parameters. The argument is always collection of receiver instances.

Queries should define user friendly #description. I provide very general implementation based on class name. Look at implementors for examples.

Navigation environment caches my instances and their results. It requires correct implementation of equality and hashing.
Some queries include various state which can be initialized at different time. It is important that instance will be not modified after execution because instead it can affect hash and equality functions which are used by cache. 
For this reason I implement special method #fixStateBeforeExecution which marks the instance and related state (the scope for example) as read only objects (#beReadOnlyObject). So after execution my instances became immutable.

And according to this logic I provide special hook #prepareStateBeforeExecution to prepare complete state of query instance before execution. It allows initialize lazy variables before making instance immutable.
			
The Calypso-Browser package provide UI widget to browse query results. For this purpose I provide helper method to open browser cursor:

	aQuery openBrowserCursorFor: anItemObserver

It is shortcut method to execute query and open cursor on result. So read details in ClyQueryResult comments.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	requestedResult:		<ClyQueryResult>
	scope:		<ClyScope>
"
Class {
	#name : #ClyQuery,
	#superclass : #Object,
	#instVars : [
		'requiredResult',
		'scope'
	],
	#classVars : [
		'ShouldBeProtectedByWriteBarrier'
	],
	#category : #'Calypso-NavigationModel-Query'
}

{ #category : #'instance creation' }
ClyQuery class >> as: aQueryResultClass [
	^self new 
		requiredResult: aQueryResultClass
]

{ #category : #accessing }
ClyQuery class >> disableWriteBarrierProtection [
	ShouldBeProtectedByWriteBarrier := false
]

{ #category : #accessing }
ClyQuery class >> enableWriteBarrierProtection [
	Smalltalk vm supportsWriteBarrier ifFalse: [ self error: 'Vm do not support write barrier' ].
	ShouldBeProtectedByWriteBarrier := true
]

{ #category : #'merging queries' }
ClyQuery class >> mergeOwnInstances: queries [
	"Subclasses should return collection of queries merged from given array.
	For example two AllClasses queries from same package scope can be merged.
	They can be represented by single AllClasses query 
	from unified scope of both packages from original queries"
	self subclassResponsibility
]

{ #category : #'instance creation' }
ClyQuery class >> sortedBy: aSortFunction [
	^self as: (ClySortedQueryResult using: aSortFunction)
]

{ #category : #'instance creation' }
ClyQuery class >> unionFrom: subqueries [
	^ClyUnionQuery with: subqueries
]

{ #category : #'instance creation' }
ClyQuery class >> unionFrom: subqueries as: aQueryResult [
	^ClyUnionQuery with: subqueries as: aQueryResult 
]

{ #category : #composition }
ClyQuery >> , anotherQuery [ 
	self subclassResponsibility 
]

{ #category : #comparing }
ClyQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class	ifFalse: [ ^ false ].
		
	^ requiredResult = anObject requiredResult	
		and: [scope = anObject scope]
]

{ #category : #converting }
ClyQuery >> async [
	^ClyAsyncQuery for: self
]

{ #category : #accessing }
ClyQuery >> bindTo: aScope in: aNavigationEnvironment [
	aScope bindTo: aNavigationEnvironment.
	self scope: aScope
]

{ #category : #execution }
ClyQuery >> buildResult: aQueryResult [
	self subclassResponsibility 
]

{ #category : #testing }
ClyQuery >> buildsDefaultResult [

	^self retrievesItemsAs: self defaultResult class
]

{ #category : #execution }
ClyQuery >> checkEmptyResult [
	self subclassResponsibility 
]

{ #category : #execution }
ClyQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	self subclassResponsibility
]

{ #category : #initialization }
ClyQuery >> defaultResult [
	^ClyRawQueryResult new
]

{ #category : #printing }
ClyQuery >> description [
	^String streamContents: [:s | 
		s nextPutAll: self class name.
		s nextPut: $(.
		self printExtraInfoOn: s.
		s nextPut: $)]
]

{ #category : #accessing }
ClyQuery >> ensureScope: aScope [
	scope == ClyUnknownScope instance ifTrue: [ ^self scope: aScope ].
	
	scope == aScope ifFalse: [ 
		self error: 'Scope modification is forbidden. Query is bound to scope forever' ]
]

{ #category : #accessing }
ClyQuery >> environment [
	scope ifNil: [ self error: 'Scope is not defined. So environment is undefined' ].
	^scope environment
]

{ #category : #execution }
ClyQuery >> execute [
	^scope query: self
]

{ #category : #testing }
ClyQuery >> executesQuery: aTypedQueryClass [
	"We should be able to check that given query executes particular kind of query class.
	It allows simple check for tools that desired kind of query is activated.
	Composite query should check all subqueries according to this logic"
	self subclassResponsibility 
]

{ #category : #converting }
ClyQuery >> filteredBy: anItemFilter [
	^ClyFilterQuery for: self filter: anItemFilter.
]

{ #category : #execution }
ClyQuery >> fixStateBeforeExecution [
	"Here the query should become readonly object together with all required internal state. By default it is only instance itself. But subclasses like composite query force additional objects (subqueries) to fix their state.
	If query needs additional state for execution 
	it should retrieve it in prepare method which is executed before readonly fix"
	self prepareStateBeforeExecution.
	self shouldBeProtectedByWriteBarrier ifFalse: [^self].
	self beReadOnlyObject.
	scope beReadOnlyObject
]

{ #category : #printing }
ClyQuery >> fullDescription [
	
	^self description, ' from ',
		(scope isBasedOnEmptyBasis ifTrue: [ 'empty scope'] ifFalse: [ scope description])
]

{ #category : #execution }
ClyQuery >> hasEmptyResult [
	^scope isQueryEmpty: self
]

{ #category : #comparing }
ClyQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^requiredResult hash bitXor: scope hash
]

{ #category : #initialization }
ClyQuery >> initialize [
	super initialize.
	self resetScope.
	self requiredResult: self defaultResult
]

{ #category : #testing }
ClyQuery >> isAsync [
	^false
]

{ #category : #testing }
ClyQuery >> isBoundToEnvironment [
	^scope isBoundToEnvironment
]

{ #category : #testing }
ClyQuery >> isExecutedFromEmptyScope [

	^scope isBasedOnEmptyBasis
]

{ #category : #testing }
ClyQuery >> isExecutedFromMultipleScope [
	^scope isBasedOnMultipleBasis
]

{ #category : #testing }
ClyQuery >> isExecutedFromScope: aTypedScopeClass [

	^scope representsScope: aTypedScopeClass
]

{ #category : #testing }
ClyQuery >> isExecutedFromScopeOf: basisObject [

	^scope isBasedOn: basisObject 
]

{ #category : #testing }
ClyQuery >> isExecutedFromSingleScope [
	^scope isBasedOnSingleBasis 
]

{ #category : #'system changes' }
ClyQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^self subclassResponsibility 
]

{ #category : #execution }
ClyQuery >> openBrowserCursorFor: anItemObserver [
	^self execute openBrowserCursorFor: anItemObserver
]

{ #category : #execution }
ClyQuery >> prepareNewResult [
	
	^requiredResult prepareNewFor: self in: self environment
]

{ #category : #execution }
ClyQuery >> prepareStateBeforeExecution [
	"Here subclasses can initialize some additional state from own instance variables
	which is required for execution"
]

{ #category : #printing }
ClyQuery >> printExtraInfoOn: aStream [

]

{ #category : #printing }
ClyQuery >> printOn: aStream [
	| position |
	super printOn: aStream.
	
	aStream nextPut: $(.
	position := aStream position.
	self printExtraInfoOn: aStream.
	aStream position = position 
		ifTrue: [ aStream skip: -1; nextPut: $  ]
		ifFalse: [ aStream nextPutAll: ') ' ].
	aStream nextPutAll: 'from '.
	scope isBasedOnEmptyBasis 
		ifTrue: [ aStream nextPutAll: 'empty scope']
		ifFalse: [ scope printDescriptionOn: aStream]. 
	
]

{ #category : #accessing }
ClyQuery >> requiredResult [
	^ requiredResult
]

{ #category : #accessing }
ClyQuery >> requiredResult: aQueryResult [
	requiredResult := aQueryResult.
	self shouldBeProtectedByWriteBarrier ifTrue: [ 
		requiredResult beReadOnlyObject ]
]

{ #category : #initialization }
ClyQuery >> resetScope [
	scope := ClyUnknownScope instance
]

{ #category : #converting }
ClyQuery >> restrictedByScope: aScope [
	self subclassResponsibility 
]

{ #category : #testing }
ClyQuery >> retrievesBrowserItems [
	^requiredResult representsBrowserItems
]

{ #category : #testing }
ClyQuery >> retrievesItem: anObject [
	self subclassResponsibility 
]

{ #category : #testing }
ClyQuery >> retrievesItemsAs: aQueryResultClass [
	^requiredResult isKindOf: aQueryResultClass 
]

{ #category : #testing }
ClyQuery >> retrievesItemsOfType: itemTypeClass [
	self subclassResponsibility 
]

{ #category : #accessing }
ClyQuery >> scope [
	^ scope
]

{ #category : #accessing }
ClyQuery >> scope: aScope [
	aScope isBoundToEnvironment ifFalse: [ 
		^self error: 'Query should be always initialized with scope bound to concrete environment' ].
	(aScope supportsQuery: self) ifFalse: [ 
		self error: 'Given scope do not support given query! Try use #adoptQuery:' ].
	
	scope := aScope
]

{ #category : #converting }
ClyQuery >> semiAsync [
	^self async 
		asyncResult: ClySemiAsyncQueryResult new
]

{ #category : #testing }
ClyQuery >> shouldBeProtectedByWriteBarrier [
	^ShouldBeProtectedByWriteBarrier ifNil: [ 
		ShouldBeProtectedByWriteBarrier := Smalltalk vm supportsWriteBarrier]
]

{ #category : #composition }
ClyQuery >> unionWith: typedQueries as: aQueryResult [
	"Subclasses should return ClyUnionQuery with collection of given subqueries together with itself or own subqueries if exists.
	To create ClyUnionQuery they can yse short version ClyQuery unionFrom:"
	self subclassResponsibility 
]

{ #category : #converting }
ClyQuery >> withResult: aQueryResult [
	| copy |
	copy := self copy.
	copy requiredResult: aQueryResult.
	^copy
]

{ #category : #converting }
ClyQuery >> withScope: aScope [
	self subclassResponsibility 
]

{ #category : #converting }
ClyQuery >> withScopeOf: newBasisObjects [
	self subclassResponsibility 
]

{ #category : #converting }
ClyQuery >> withoutItemsOfType: anItemTypeClass [
	self subclassResponsibility 
]
