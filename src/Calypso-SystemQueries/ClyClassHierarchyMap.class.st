"
I represent the actual relationship between classes using simple map between parents and children.
Concrete type of hierarchy build my instances where it defines what classes should be parent and what should be children:

	aHierarchyMap addChild: childClass to: parentClass.

My instances are created by hierarchies using following method: 

	map := ClyClassHierarchyMap for: aHierarchy of: classes.

And my method #build initiates actual map building. 

Users do not need to create map directly. Instead they ask concrete hierarchy to build from classes: 

	map := ClySubclassHierarchy buildFrom: classes.

During building I collect roots which are used as a starting point to access classes in hierarchical order:

	map doInOrder: [ :class |  ]
	map doInOrderWithDepth: [ :class :depth | ]

You can also iterate roots: 

	map rootsDo: [ :class |  ]

Childen of every class are sorted according to the sort function of the hierarchy.

Internal Representation and Key Implementation Points.

    Instance Variables
	classes:		<IdentitySet<Class>>
	hierarchy:		<ClyClassHierarchy>
	parentMap:		<Dictionary<Class, SortedCollection<Class>>>
	roots:		<IdentitySet<Class>>
"
Class {
	#name : #ClyClassHierarchyMap,
	#superclass : #Object,
	#instVars : [
		'hierarchy',
		'classes',
		'roots',
		'parentMap'
	],
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #'instance creation' }
ClyClassHierarchyMap class >> for: aClassHierarchy of: classes [
	^self new 
		hierarchy: aClassHierarchy;
		classes: classes
]

{ #category : #private }
ClyClassHierarchyMap >> addActualChild: childClass to: parentClass [
 
	| children |
	children := parentMap at: parentClass ifAbsentPut: [	self containerForChildren].
	children add: childClass.
	roots remove: childClass ifAbsent: nil
]

{ #category : #accessing }
ClyClassHierarchyMap >> addChild: childClass to: parentClass [
 
	hierarchy isInverse 
		ifTrue: [ self addActualChild: parentClass to: childClass ]
		ifFalse: [ self addActualChild: childClass to: parentClass ]
]

{ #category : #accessing }
ClyClassHierarchyMap >> allChildrenOf: aClass depth: currentDepth do: blockWithClass [

	| children childDepth |
	children := parentMap at: aClass ifAbsent: [ ^self ].
	childDepth := currentDepth + 1.
	children do: [:eachChild | 
		blockWithClass value: eachChild value: childDepth.
		self allChildrenOf: eachChild depth: childDepth do: blockWithClass]
]

{ #category : #accessing }
ClyClassHierarchyMap >> allChildrenOf: aClass do: blockWithClass [

	| children |
	children := parentMap at: aClass ifAbsent: [ ^self ].
	children do: [:eachChild | 
		blockWithClass value: eachChild.
		self allChildrenOf: eachChild do: blockWithClass]
]

{ #category : #building }
ClyClassHierarchyMap >> build [
	
	roots := IdentitySet withAll: classes.
	parentMap := IdentityDictionary new.
	
	classes do: [ :each |	hierarchy buildParentMap: self for: each]
]

{ #category : #accessing }
ClyClassHierarchyMap >> childrenOf: aClass [
	^parentMap at: aClass ifAbsent: [ #() ]
]

{ #category : #accessing }
ClyClassHierarchyMap >> classes [
	^ classes
]

{ #category : #accessing }
ClyClassHierarchyMap >> classes: classCollection [
	classes := classCollection asIdentitySet 
]

{ #category : #accessing }
ClyClassHierarchyMap >> containerForChildren [
	^hierarchy containerForChildren
]

{ #category : #accessing }
ClyClassHierarchyMap >> doInOrder: blockWithClass [
	
	self rootsDo: [ :root | 
		blockWithClass value: root.
		self allChildrenOf: root do: blockWithClass ]
]

{ #category : #accessing }
ClyClassHierarchyMap >> doInOrderWithDepth: blockWithClassAndDepth [
	
	self rootsDo: [ :root | 
		blockWithClassAndDepth value: root value: 0.
		self allChildrenOf: root depth: 0 do: blockWithClassAndDepth ]
]

{ #category : #accessing }
ClyClassHierarchyMap >> hierarchy [
	^ hierarchy
]

{ #category : #accessing }
ClyClassHierarchyMap >> hierarchy: anObject [
	hierarchy := anObject
]

{ #category : #testing }
ClyClassHierarchyMap >> includesClass: aClass [
	^classes includes: aClass
]

{ #category : #initialization }
ClyClassHierarchyMap >> initialize [
	super initialize.
	parentMap := IdentityDictionary new.
	
]

{ #category : #accessing }
ClyClassHierarchyMap >> roots [
	^roots
]

{ #category : #accessing }
ClyClassHierarchyMap >> rootsDo: aBlock [

	self containerForChildren 
		addAll: roots;
		do: aBlock
]
