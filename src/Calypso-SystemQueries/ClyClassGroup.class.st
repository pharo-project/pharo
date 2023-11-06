"
I am a root of hierarchy of class groups.

Any class group is created on class query:

	ClyClassGroup named: 'some group' on: aClassQuery

Class query can be also composite but it should return classes.

Also I can be expanded to subgroups using subgroupsQuery. You can specify it in another instance creation method: 

	ClyClassGroup named: 'some group' on: aClassQuery withSubgroupsFrom: aQuery	

And there are additional constructors to specify priority of group:

	ClyClassGroup named: 'some group' priority: 20 on: aClassQuery.
	ClyClassGroup named: 'some group' priority: 20 on: aClassQuery withSubgroupsFrom: aQuery

All groups are sorted by priority and name in the browser. Larger priority value put group to the top of list.

I provide several methods to implement various commands: 

- importClass: aClass
It supposed to modify given aClass in the way that it will become the part of group.

- importClasses: classes 
It imports multiple classes
		 
- removeWithClasses
It removes all classes and should ensure that groups will be removed too which is true for all virtual groups.

Internal Representation and Key Implementation Points.

    Instance Variables
	classQuery:		<ClyClassQuery>
	subgroupsQuery:		<ClyQuery>
"
Class {
	#name : 'ClyClassGroup',
	#superclass : 'ClyItemGroup',
	#instVars : [
		'classQuery',
		'subgroupsQuery'
	],
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'item decoration' }
ClyClassGroup class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofClassGroup: aBrowserItem actualObject
]

{ #category : 'testing' }
ClyClassGroup class >> isBasedOnPackageTag [
	^false
]

{ #category : 'instance creation' }
ClyClassGroup class >> named: aString on: aClassQuery [
	^(self named: aString)
		classQuery: aClassQuery
]

{ #category : 'instance creation' }
ClyClassGroup class >> named: aString on: aClassQuery  withSubgroupsFrom: subgroupsQuery [
	^(self named: aString on: aClassQuery)
		subgroupsQuery: subgroupsQuery
]

{ #category : 'instance creation' }
ClyClassGroup class >> named: aString priority: aNumber on: aClassQuery [
	^(self named: aString priority: aNumber)
		classQuery: aClassQuery
]

{ #category : 'instance creation' }
ClyClassGroup class >> named: aString priority: aNumber on: aClassQuery withSubgroupsFrom: subgroupsQuery [
	^(self named: aString priority: aNumber on: aClassQuery)
		subgroupsQuery: subgroupsQuery
]

{ #category : 'accessing' }
ClyClassGroup class >> prepareClassQueriesFrom: classGroups in: aNavigationEnvironment [

	^ classGroups collect: [ :each | each classQuery ]
]

{ #category : 'comparing' }
ClyClassGroup >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class	ifFalse: [ ^ false ].
	^ classQuery = anObject classQuery
]

{ #category : 'adding' }
ClyClassGroup >> addClass: aClass [

	| newPackages |
	newPackages := OrderedCollection new.
	classQuery scope packagesDo: [ :package | package = aClass package ifFalse: [ newPackages add: package ] ].
	newPackages size > 1 ifTrue: [ self error: 'You should select single package for import!' ].
	newPackages ifNotEmpty: [ aClass package: newPackages anyOne ].
	aClass packageTag: self tag
]

{ #category : 'accessing' }
ClyClassGroup >> classQuery [
	^ classQuery
]

{ #category : 'accessing' }
ClyClassGroup >> classQuery: anObject [
	classQuery := anObject
]

{ #category : 'accessing' }
ClyClassGroup >> classes [
	^classQuery execute items
]

{ #category : 'accessing' }
ClyClassGroup >> classesSize [
	^classQuery execute size
]

{ #category : 'decoration' }
ClyClassGroup >> decorateOwnBrowserItem: myItem [
	super decorateOwnBrowserItem: myItem.

	classQuery decorateItemGroup: myItem.
	subgroupsQuery decorateItemGroup: myItem
]

{ #category : 'comparing' }
ClyClassGroup >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ classQuery hash
]

{ #category : 'initialization' }
ClyClassGroup >> initialize [
	super initialize.

	subgroupsQuery := ClyUnknownQuery instance
]

{ #category : 'printing' }
ClyClassGroup >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	classQuery printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'operations' }
ClyClassGroup >> removeWithClasses [
	self classes do: [ :each | each removeFromSystem ]
]

{ #category : 'accessing' }
ClyClassGroup >> subgroupsQuery [
	^ subgroupsQuery
]

{ #category : 'accessing' }
ClyClassGroup >> subgroupsQuery: anObject [
	subgroupsQuery := anObject
]

{ #category : 'accessing' }
ClyClassGroup >> tag [

	^ self subclassResponsibility
]
