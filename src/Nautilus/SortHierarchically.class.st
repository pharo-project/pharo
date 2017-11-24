"
A SortHierarchically is an algorithm used to display a list of classes where subclasses are indented and follow their superclass.

Instance Variables
	hierarchy: an array of SortHierarchicallyNodes correctly sorted

"
Class {
	#name : #SortHierarchically,
	#superclass : #Object,
	#instVars : [
		'hierarchy'
	],
	#category : #Nautilus
}

{ #category : #'instance creation' }
SortHierarchically class >> buildHierarchyFor: aPackageGroup in: anEnvironment [
	^ aPackageGroup withClassesIn: anEnvironment createHierarchySorter: self 
]

{ #category : #'instance creation' }
SortHierarchically class >> buildHierarchyForClass: aClass [
	| tupleBlock heirarchy indentDepth |
	
	indentDepth := 0.
	
	heirarchy := OrderedCollection new.
	aClass allSuperclasses reversed do: [ :superClass | 
		heirarchy add: (SortHierarchicallyIndentNode on: superClass indentBy: indentDepth). 
		indentDepth := indentDepth + 1
	].
	
	tupleBlock := nil.
	tupleBlock := [ :aTuple :indent | 
		heirarchy add: (SortHierarchicallyIndentNode on: aTuple first indentBy: indent). 
		aTuple second do: [ :tuple | tupleBlock value: tuple value: indent + 1 ]
	].
	tupleBlock value: aClass hierarchyClassTree value: indentDepth.
		
	^ self new
		setHierarchy: heirarchy;
		yourself
]

{ #category : #'instance creation' }
SortHierarchically class >> buildHierarchyForClasses: aCollection [
	
	| classes roots dct indenter result |

	indenter := nil.
	classes := aCollection asIdentitySet.
	roots := OrderedCollection new.
	dct := IdentityDictionary new.		
	
	classes do: [ :cls | | su found |
		su := cls .
		found := false.
		[ found or: [ su isNil ] ] whileFalse: [ 
			su := su superclass.
			(classes includes: su) ifTrue: [ 
					found := true.
					(dct at: su ifAbsentPut: [ OrderedCollection new ]) add: cls.
				].
		].
		found ifFalse: [ roots add: cls ].
	].

	result := OrderedCollection new.
	indenter := [ :col :indent | 
		(col sort: [ :a :b | a name <= b name ]) do: [ :each | 
			result add: (SortHierarchicallyIndentNode on: each indentBy: indent). 
			dct at: each ifPresent: [ :subCol | indenter value: subCol value: indent + 1 ]
		] 
	].

	indenter value: roots value: 0.

	^ self new
		setHierarchy: result;
		yourself
]

{ #category : #actions }
SortHierarchically class >> sortClasses: classes [
	
	^ (self buildHierarchyForClasses: classes) sortedElements 
]

{ #category : #combining }
SortHierarchically >> , another [ 

	^ self class new
		setHierarchy: (self hierarchy, another hierarchy);
		yourself
]

{ #category : #accessing }
SortHierarchically >> hierarchy [
	^ hierarchy
]

{ #category : #computation }
SortHierarchically >> indentationFor: anElement [
	"Return the number of spaces needed before anElement to indent it properly."
	"Speed could be enhanced by using a hash-based collection instead of this #detect:."
	^ (hierarchy detect: [ :node | node element = anElement ]) indent
]

{ #category : #'initialize-release' }
SortHierarchically >> setHierarchy: nodes [
	hierarchy := nodes
]

{ #category : #computation }
SortHierarchically >> sortedElements [
	"Return the list of elements sorted appropriately."
	"Nodes are already sorted, we just return each's element."
	^ hierarchy collect: [ :each | each element ]
]
