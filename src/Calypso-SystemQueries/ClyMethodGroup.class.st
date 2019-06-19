"
I am a root of hierarchy of method groups.

Any method group is created on method query:

	ClyMethodGroup named: 'some group' on: aMetodQuery

Method query can be also composite but it should return methods.

Also method group can be expanded to subgroups using subgroupsQuery. You can specify it in another instance creation method: 

	ClyMethodGroup named: 'some group' on: aMethodQuery withSubgroupsFrom: aQuery	

And there are additional constructors to specify priority of group:

	ClyMethodGroup named: 'some group' priority: 20 on: aMethodQuery.
	ClyMethodGroup named: 'some group' priority: 20 on: aMethodQuery withSubgroupsFrom: aQuery

All groups are sorted by priority and name in the browser. Larger priority value put group to the top of list.

I provide several methods to implement various commands: 

- importMethod: aMethod
It supposed to modify given aMethod in the way that it will become the part of the group.

- importMethods: methods 
It imports multiple methods
		 
- removeWithMethods
It removes all methods and should ensure that groups will be removed too which is true for all virtual groups.

And I provide method #includesMethod: which is used in the browser to highlight groups which contains selected methods.

Internal Representation and Key Implementation Points.

    Instance Variables
	methodQuery:		<ClyMethodQuery>
	subgroupsQuery:		<ClyQuery>
"
Class {
	#name : #ClyMethodGroup,
	#superclass : #ClyItemGroup,
	#instVars : [
		'methodQuery',
		'subgroupsQuery'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'item decoration' }
ClyMethodGroup class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofMethodGroup: aBrowserItem actualObject
]

{ #category : #testing }
ClyMethodGroup class >> isBasedOnMethodTag [
	^false
]

{ #category : #'instance creation' }
ClyMethodGroup class >> named: aString on: aMethodQuery [
	^(self named: aString)
		methodQuery: aMethodQuery
]

{ #category : #'instance creation' }
ClyMethodGroup class >> named: aString on: aMethodQuery withSubgroupsFrom: subgroupsQuery [
	^(self named: aString on: aMethodQuery) 
		subgroupsQuery: subgroupsQuery
]

{ #category : #'instance creation' }
ClyMethodGroup class >> named: aString priority: aNumber on: aMethodQuery [
	^(self named: aString priority: aNumber)
		methodQuery: aMethodQuery
]

{ #category : #'instance creation' }
ClyMethodGroup class >> named: aString priority: aNumber on: aMethodQuery withSubgroupsFrom: subgroupsQuery [
	^(self named: aString priority: aNumber on: aMethodQuery) 
		subgroupsQuery: subgroupsQuery
]

{ #category : #queries }
ClyMethodGroup class >> prepareSubgroupsQueryFrom: aMethodGroup in: aNavigationEnvironment [

	^aMethodGroup subgroupsQuery
]

{ #category : #queries }
ClyMethodGroup class >> shortName [
	^'Methods'
]

{ #category : #comparing }
ClyMethodGroup >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	
	^ methodQuery = anObject methodQuery
		and: [ subgroupsQuery = anObject subgroupsQuery ]
]

{ #category : #testing }
ClyMethodGroup >> asAsyncQueryGroup [
	^ClyAsyncMethodGroup 
		named: name 
		priority: priority 
		on: methodQuery
		withSubgroupsFrom: subgroupsQuery
]

{ #category : #operations }
ClyMethodGroup >> convertToMethodTag: aTagName [

	self methods do: [ :each | each tagWith: aTagName ]
]

{ #category : #decoration }
ClyMethodGroup >> decorateOwnBrowserItem: myItem [
	super decorateOwnBrowserItem: myItem.
	
	methodQuery decorateItemGroup: myItem.
	subgroupsQuery decorateItemGroup: myItem.
]

{ #category : #testing }
ClyMethodGroup >> dependsOnMethod: aMethod [
	"This method is used to collect dynamic method group which depends on methods.
	It can be simple #includesMethod: which is default implementation here.
	But some groups require hook to be in group list but do not includes any methods.
	They will override it with true return. For example look at async method group"
	^self includesMethod: aMethod
]

{ #category : #comparing }
ClyMethodGroup >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ methodQuery hash bitXor: subgroupsQuery hash
]

{ #category : #operations }
ClyMethodGroup >> importMethod: aMethod [ 

]

{ #category : #operations }
ClyMethodGroup >> importMethods: methodsCollection [ 

	methodsCollection do: [ :each | self importMethod: each ]
]

{ #category : #testing }
ClyMethodGroup >> includesMethod: aMethod [
	^self methodQuery retrievesItem: aMethod
]

{ #category : #initialization }
ClyMethodGroup >> initialize [
	super initialize.
	
	subgroupsQuery := ClyUnknownQuery instance
]

{ #category : #'system changes' }
ClyMethodGroup >> isAffectedByPackageChange: aPackageAnnouncement [
	^false
]

{ #category : #testing }
ClyMethodGroup >> isEmpty [
	^methodQuery hasEmptyResult 
]

{ #category : #accessing }
ClyMethodGroup >> methodQuery [
	^methodQuery
]

{ #category : #accessing }
ClyMethodGroup >> methodQuery: aMethodQuery [
	methodQuery := aMethodQuery
]

{ #category : #accessing }
ClyMethodGroup >> methods [
	^methodQuery execute items
]

{ #category : #accessing }
ClyMethodGroup >> methodsSize [
	^self methods size
]

{ #category : #printing }
ClyMethodGroup >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	methodQuery printOn: aStream.
	aStream nextPut: $).
]

{ #category : #operations }
ClyMethodGroup >> removeWithMethods [
	self methods do: [ :each | each removeFromSystem ]
]

{ #category : #accessing }
ClyMethodGroup >> subgroupsQuery [
	^subgroupsQuery
]

{ #category : #accessing }
ClyMethodGroup >> subgroupsQuery: anObject [
	subgroupsQuery := anObject
]
