"
I am a special kind of system query result which merges together multiple kind of system items in single hierarchy.
So I sort hierarchically classes, methods and class comments, all together.

I am used in query browser to show items.

First I build subclass hierarchy of all classes with define given objects.
For this I convert every object to the browser item and ask it systemDefinitionClass.
At the end I got map between class and children items which are defined by this class. 
The children are sorted using special ClySortSystemItemFunction which takes into account different type of items and their own default sort logic.

Using this built hierarchy I fill my items in hierarchical order.
"
Class {
	#name : #ClyHierarchicalSystemItems,
	#superclass : #ClyBrowserQueryResult,
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #building }
ClyHierarchicalSystemItems >> buildItemsPerClassFrom: systemObjects [
	| itemsPerClass item localItems |
	itemsPerClass := IdentityDictionary new.

	systemObjects do: [ :each |
		item := each asCalypsoBrowserItem.
		localItems := itemsPerClass
			at: item systemDefinitionClass ifAbsentPut: [self containerForItemsOfClass ].
		localItems add: item].
	
	^itemsPerClass
]

{ #category : #building }
ClyHierarchicalSystemItems >> containerForItemsOfClass [
	^SortedCollection sortBlock: ClySortSystemItemFunction ascending forBrowserItems
]

{ #category : #building }
ClyHierarchicalSystemItems >> fillWith: objects [
	"System objects supposed to have definition class and package.
	It can be classes, methods, variables, class comments and so on"
	| itemsPerClass localItems classMap |
	itemsPerClass := self buildItemsPerClassFrom: objects.
	classMap := ClySubclassHierarchy buildFrom: itemsPerClass keys.
	
	items := OrderedCollection new: objects size.
	classMap doInOrderWithDepth: [ :eachClass :depth |
		localItems := itemsPerClass at: eachClass.
		localItems do: [:each | each depth: depth].
		items addAll: localItems].
]
