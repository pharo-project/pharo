"
I am a Strategy that will use the function on all the tree.
BE CAREFUL, do NOT use me if you are not sur that your tree is finish. If a branch is infinite I will go in an infinite loop.
 If you are sure about it, use this otherwise use other search strategies. 

Description
-----------------------

I will search in every item of the tree and I needed I will expand the items to show the results of the function.
As said in my superclass I am use by a FTTreeDataSource to help with a FTFunction.

Public API and Key Messages
-----------------------

- #realSearch 	is the method that will launch the search.

Example
-----------------------

	| ds |
	ds := FTTreeDataSource
		roots:
			((ProtoObject allSubclasses sort: [ :a :b | a asString < b asString ])
				reject: [ :e | e asString endsWith: 'class' ])
		children: [ :item | item subclasses sort: [ :a :b | a asString < b asString ] ].
	ds searchStrategy: #allItems. ""This will say to the FTTreeDataSource to use me.""
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
 
Internal Representation and Key Implementation Points.
-----------------------

    Instance Variables
	index:		I am the index of tfe elements I am testing now. 
	result:		I am a collection of index that is the result of the search.

"
Class {
	#name : #FTAllItemsStrategy,
	#superclass : #FTTreeFunctionStrategy,
	#instVars : [
		'index',
		'result'
	],
	#category : #'Morphic-Widgets-FastTable-Tree'
}

{ #category : #action }
FTAllItemsStrategy >> filter [
	| items |
	items := OrderedCollection new.
	dataSource rootsItems do: [ :item | (self matchingFilter: item) ifNotNil: [ :itemNew | items add: itemNew ] ] displayingProgress: [ :each | 'Looking inside ' , each printString ].
	dataSource table selectRowIndex: 1.
	^ dataSource class
		root:
			(FTRootItem new
				children: items asArray;
				yourself)
		children: dataSource childrenBlock
]

{ #category : #action }
FTAllItemsStrategy >> initializeNewSearch [
	result := OrderedCollection new.
	index := 1
]

{ #category : #action }
FTAllItemsStrategy >> matchingFilter: anItem [
	((anItem children collect: [ :it | self matchingFilter: it ]) reject: #isNil)
		ifNotEmpty:
			[ :coll | 
			^ (self newStaticItemFrom: anItem)
				children: coll asArray;
				dataSource: anItem dataSource;
				expandWithoutChange;
				yourself ].
	^ (pattern matchesIn: (self dataSource toString: anItem)) ifEmpty: [ nil ] ifNotEmpty: [ self newStaticItemFrom: anItem ]
]

{ #category : #action }
FTAllItemsStrategy >> newStaticItemFrom: anItem [
	^ FTStaticBasicItem new
		data: anItem data;
		depth: anItem depth;
		children: #();
		yourself
]

{ #category : #action }
FTAllItemsStrategy >> optionalOperations [
	pattern isEmptyOrNil
		ifTrue: [ self unexpandAllChildOf: dataSource rootItem.
			^ #() ]
		ifFalse: [ ^ nil ]
]

{ #category : #action }
FTAllItemsStrategy >> realSearch [
	self initializeNewSearch.
	dataSource rootsItems do: [ :item | self searchIn: item ] displayingProgress: [ :each | 'Looking inside ' , each name ].
	^ result asArray
]

{ #category : #action }
FTAllItemsStrategy >> searchIn: item [
	| selfOrChildMatch |
	selfOrChildMatch := false.
	(self isMatching: item)
		ifTrue: [ result add: index.
			selfOrChildMatch := true ].
	index := index + 1.
	(item hasChildren and: [ dataSource canDisplayChildrenOf: item ])
		ifTrue:
			[ | itemIndex |
			itemIndex := index.	"I save this index because if there is no match inside this item I should not expand it."
			(item children select: [ :child | self searchIn: child ])
				ifNotEmpty: [ selfOrChildMatch := true.
					item expand ]
				ifEmpty: [ item collapse.
					index := itemIndex ] ].
	^ selfOrChildMatch
]

{ #category : #action }
FTAllItemsStrategy >> unexpandAllChildOf: anItem [
	anItem children
		do: [ :item | 
			item isExpanded
				ifTrue: [ self unexpandAllChildOf: item ].
			item collapse ]  displayingProgress: [:each | 'Rearanging ' , each name]
]
