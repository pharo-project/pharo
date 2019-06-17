"
My subclasses provide model of different kind of class hierarchies.
They define concrete relationship between classes which are used to build hierarchies.
The result of build is an instance of ClyClassHierarchyMap which provides dictionary between parent classes and their children.
To build map instance use following message: 

	map := ClySubclassHierarchy new buildFrom: { Object. Array }.

or simply ask class for this:

	map := ClySubclassHierarchy buildFrom: { Object. Array }.

With hierarchy map you can iterate classes in hierarchical order: 

	map doInOrder: [ :class | ].
	map doInOrderWithDepth: [ :class :depth | ].

And you can ask map for children of class: 

	map childrenOf: Object.
	
Or you can also access the roots: 

	map rootsDo: [ :class | ]

The children are collected as sorted list which order is defined by #sortFunction.
So you can build hierarchies where children are sorted by different criterias. By default they are sorted by class name in ascending order.
To instantiate hierarchy with another sort function use following expression:
	
	ClySubclassHierarchy sortedBy: aSortFunction 
	
And you can build sorted hierarchy using short class side method:

	ClySubclassHierarchy buildFrom: Smalltalk allClasses sortedBy: ClySortByNameFunction descending.

It will sort children by class name in reversed order.

And you can convert any hierarchy to new sorted version: 

	newHierarchy := aHierarchy sortedBy: aSortFunction.

So subclasses define relationship between classes. They should implement following method:

- buildParentMap: aHierarchyMap for: aClass

where they add every pair parent and aClass to the map. They should do it using following method: 

	aHierarchyMap addChild: aClass to: eachParentClass

Look at implementors of #buildParentMap:for: for examples.

The relationship which defined by each subclass arranges kind of natural order of hierarchy. But it can be inverted.
For example natural order of ClySubclassHierarchy will put all common superclasses to the roots of hierarchy.
And inverse version will put all leaf subclasses to the roots.

I encode flag inverse logic in the variable #inverse. You can create inverse hierarchies with following expression: 

	ClySubclassHierarchy inverse.

Or you can build inverse hierarchy with short class side methods: 

	ClySubclassHierarchy buildInverseFrom: {Object. String}.
	ClySubclassHierarchy buildInverseFrom: {Object. String} sortedBy: aSortFunction.

You can see in inspector that in first expression the root is String class.

And you can convert any hierarchy to the inverted version: 

	newHierarchy := aHierarchy inverted.

Also I provide converting method to create query result which you can pass to the queries: 

	ClyAllClasses as: ClySubclassHierarchy inverse asQueryResult 

Internal Representation and Key Implementation Points.

    Instance Variables
	inverse:		<Boolean>
	sortFunction:		<SortFunction>
"
Class {
	#name : #ClyClassHierarchy,
	#superclass : #Object,
	#instVars : [
		'sortFunction',
		'inverse'
	],
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #building }
ClyClassHierarchy class >> buildFrom: classes [
	^self buildFrom: classes sortedBy: ClySortByNameFunction ascending
]

{ #category : #building }
ClyClassHierarchy class >> buildFrom: classes sortedBy: aSortFunction [
	^(self sortedBy: aSortFunction)
		buildFrom: classes
]

{ #category : #building }
ClyClassHierarchy class >> buildInverseFrom: classes [
	^self buildInverseFrom: classes sortedBy: ClySortByNameFunction ascending
]

{ #category : #building }
ClyClassHierarchy class >> buildInverseFrom: classes sortedBy: aSortFunction [

	^(self sortedBy: aSortFunction)
		inverse: true;
		buildFrom: classes
]

{ #category : #'instance creation' }
ClyClassHierarchy class >> inverse [
	^self new 
		inverse: true
]

{ #category : #'instance creation' }
ClyClassHierarchy class >> new [
	^super new 
		sortFunction: ClySortByNameFunction ascending
]

{ #category : #'instance creation' }
ClyClassHierarchy class >> sortedBy: aSortFunction [
	^super new 
		sortFunction: aSortFunction
]

{ #category : #composition }
ClyClassHierarchy >> , anotherHierarchy [
	^ClyMergedHierarchy merge: { self. anotherHierarchy }
]

{ #category : #comparing }
ClyClassHierarchy >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ sortFunction = anObject sortFunction
		and: [ inverse = anObject isInverse ]
]

{ #category : #converting }
ClyClassHierarchy >> asQueryResult [

	^ClyHierarchicallySortedClasses with: self
]

{ #category : #building }
ClyClassHierarchy >> buildFrom: classes [
	| map |
	map := ClyClassHierarchyMap for: self of: classes.
	map build.
	^map
]

{ #category : #building }
ClyClassHierarchy >> buildParentMap: aHierarchyMap for: aClass [
	self subclassResponsibility 
]

{ #category : #building }
ClyClassHierarchy >> containerForChildren [
	^SortedCollection sortBlock: sortFunction
]

{ #category : #comparing }
ClyClassHierarchy >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ sortFunction hash bitXor: inverse hash
]

{ #category : #initialization }
ClyClassHierarchy >> initialize [
	super initialize.
	inverse := false
]

{ #category : #accessing }
ClyClassHierarchy >> inverse: aBoolean [
	inverse := aBoolean
]

{ #category : #converting }
ClyClassHierarchy >> inverted [
	^self copy 
		inverse: inverse not
]

{ #category : #testing }
ClyClassHierarchy >> isInverse [
	^inverse
]

{ #category : #printing }
ClyClassHierarchy >> printOn: aStream [
	super printOn: aStream.
	
	inverse ifTrue: [ aStream nextPutAll: '(inverse)']
]

{ #category : #accessing }
ClyClassHierarchy >> sortFunction [
	^ sortFunction
]

{ #category : #accessing }
ClyClassHierarchy >> sortFunction: anObject [
	sortFunction := anObject
]

{ #category : #converting }
ClyClassHierarchy >> sortedBy: aSortFunction [
	^self copy 
		sortFunction: aSortFunction
]
