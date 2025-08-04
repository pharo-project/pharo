"
I am the root of the method group hierarchy.

 method group is created on a method query:

	ClyMethodGroup named: 'some group' on: aMethodQuery

A method query can also be a composite, and should return methods in any case.

Also, a method group can be expanded into subgroups using subgroupsQuery, which you can specify using this other instance creation method: 

	ClyMethodGroup named: 'some group' on: aMethodQuery withSubgroupsFrom: aQuery	

There are additional constructors to specify the group's priority:

	ClyMethodGroup named: 'some group' priority: 20 on: aMethodQuery.
	ClyMethodGroup named: 'some group' priority: 20 on: aMethodQuery withSubgroupsFrom: aQuery

In the browser, groups are sorted by descending priority and then by name.

I provide several methods to implement various commands: 

- importMethod: aMethod
It is supposed to modify aMethod, making it part of the group.

- importMethods: methods 
Same as above, but for multiple methods
		 
- removeWithMethods
It removes all methods and should ensure that groups will be removed too which is true for all virtual groups.

And I provide #includesMethod:, which is used in the browser to highlight groups which contains selected method(s).

Internal Representation and Key Implementation Points.

    Instance Variables
	methodQuery:		<ClyMethodQuery>
	subgroupsQuery:		<ClyQuery>
"
Class {
	#name : 'ClyMethodGroup',
	#superclass : 'ClyItemGroup',
	#instVars : [
		'methodQuery',
		'subgroupsQuery'
	],
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'item decoration' }
ClyMethodGroup class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofMethodGroup: aBrowserItem actualObject
]

{ #category : 'testing' }
ClyMethodGroup class >> isBasedOnExtensions [

	^ false
]

{ #category : 'testing' }
ClyMethodGroup class >> isBasedOnProtocol [
	^false
]

{ #category : 'instance creation' }
ClyMethodGroup class >> named: aString on: aMethodQuery [
	^(self named: aString)
		methodQuery: aMethodQuery
]

{ #category : 'instance creation' }
ClyMethodGroup class >> named: aString on: aMethodQuery withSubgroupsFrom: subgroupsQuery [
	^(self named: aString on: aMethodQuery)
		subgroupsQuery: subgroupsQuery
]

{ #category : 'instance creation' }
ClyMethodGroup class >> named: aString priority: aNumber on: aMethodQuery [
	^(self named: aString priority: aNumber)
		methodQuery: aMethodQuery
]

{ #category : 'instance creation' }
ClyMethodGroup class >> named: aString priority: aNumber on: aMethodQuery withSubgroupsFrom: subgroupsQuery [
	^(self named: aString priority: aNumber on: aMethodQuery)
		subgroupsQuery: subgroupsQuery
]

{ #category : 'queries' }
ClyMethodGroup class >> prepareSubgroupsQueryFrom: aMethodGroup in: aNavigationEnvironment [

	^aMethodGroup subgroupsQuery
]

{ #category : 'queries' }
ClyMethodGroup class >> shortName [
	^'Methods'
]

{ #category : 'comparing' }
ClyMethodGroup >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].

	^ methodQuery = anObject methodQuery
		and: [ subgroupsQuery = anObject subgroupsQuery ]
]

{ #category : 'testing' }
ClyMethodGroup >> asAsyncQueryGroup [
	^ClyAsyncMethodGroup
		named: name
		priority: priority
		on: methodQuery
		withSubgroupsFrom: subgroupsQuery
]

{ #category : 'operations' }
ClyMethodGroup >> categorizeMethodsIn: protocolName [

	self methods do: [ :method | method protocol: protocolName ]
]

{ #category : 'decoration' }
ClyMethodGroup >> decorateOwnBrowserItem: myItem [
	super decorateOwnBrowserItem: myItem.

	methodQuery decorateItemGroup: myItem.
	subgroupsQuery decorateItemGroup: myItem
]

{ #category : 'testing' }
ClyMethodGroup >> dependsOnMethod: aMethod [
	"This method is used to collect dynamic method group which depends on methods.
	It can be simple #includesMethod: which is default implementation here.
	But some groups require hook to be in group list but do not includes any methods.
	They will override it with true return. For example look at async method group"
	^self includesMethod: aMethod
]

{ #category : 'comparing' }
ClyMethodGroup >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ methodQuery hash bitXor: subgroupsQuery hash
]

{ #category : 'operations' }
ClyMethodGroup >> importMethod: aMethod [
]

{ #category : 'operations' }
ClyMethodGroup >> importMethods: methodsCollection [

	methodsCollection do: [ :each | self importMethod: each ]
]

{ #category : 'testing' }
ClyMethodGroup >> includesMethod: aMethod [
	^self methodQuery retrievesItem: aMethod
]

{ #category : 'initialization' }
ClyMethodGroup >> initialize [
	super initialize.

	subgroupsQuery := ClyUnknownQuery instance
]

{ #category : 'system changes' }
ClyMethodGroup >> isAffectedByPackageChange: aPackageAnnouncement [
	^false
]

{ #category : 'testing' }
ClyMethodGroup >> isEmpty [
	^methodQuery hasEmptyResult
]

{ #category : 'accessing' }
ClyMethodGroup >> methodQuery [
	^methodQuery
]

{ #category : 'accessing' }
ClyMethodGroup >> methodQuery: aMethodQuery [
	methodQuery := aMethodQuery
]

{ #category : 'accessing' }
ClyMethodGroup >> methods [
	^methodQuery execute items
]

{ #category : 'accessing' }
ClyMethodGroup >> methodsSize [
	^self methods size
]

{ #category : 'printing' }
ClyMethodGroup >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	methodQuery printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'operations' }
ClyMethodGroup >> removeProtocolFrom: aCollectionOfClasses [
	"Do nothing at this level."

	
]

{ #category : 'operations' }
ClyMethodGroup >> removeWithMethods [
	self methods do: [ :each | each removeFromSystem ]
]

{ #category : 'accessing' }
ClyMethodGroup >> subgroupsQuery [
	^subgroupsQuery
]

{ #category : 'accessing' }
ClyMethodGroup >> subgroupsQuery: anObject [
	subgroupsQuery := anObject
]
