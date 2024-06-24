"
I represent the root item node in a FTTreeDataSource.

Description
-------------------------------------------------

I am store by a FTTreeDataSource and my children are the roots elements of the dataSource.
I store the a collection of Items build from the data of the dataSource.


Example
-------------------------------------------------
 
FTRootItem data: Object allSubclasses from: aDataSource
"
Class {
	#name : 'FTRootItem',
	#superclass : 'FTTreeItem',
	#category : 'Morphic-Widgets-FastTable-Tree',
	#package : 'Morphic-Widgets-FastTable',
	#tag : 'Tree'
}

{ #category : 'accessing' }
FTRootItem >> children [
	^ children
]

{ #category : 'accessing' }
FTRootItem >> children: aCollectionOfItem [
	children := aCollectionOfItem
]

{ #category : 'expanding-collapsing' }
FTRootItem >> collapseAll [
	children do: #collapseAll
]

{ #category : 'accessing' }
FTRootItem >> data: aCollection [
	self children: (aCollection collect: [ :object | FTBasicItem data: object from: dataSource ]) asArray
]

{ #category : 'accessing' }
FTRootItem >> dataSource: anObject [
	dataSource := anObject.
	children ifNotNil: [ children do: [ :item | item dataSource: anObject ] ]
]

{ #category : 'expanding-collapsing' }
FTRootItem >> expandAll [
	children do: #expandAll
]

{ #category : 'expanding-collapsing' }
FTRootItem >> expandAllTo: aDepth [
	children do: [ :each | each expandAllTo: aDepth ]
]

{ #category : 'testing' }
FTRootItem >> isExpanded [
	^ true
]

{ #category : 'testing' }
FTRootItem >> isRoot [
	^ true
]

{ #category : 'accessing' }
FTRootItem >> recentlyChanged [
	"Cannot collapse root item."

	^ false
]

{ #category : 'updating' }
FTRootItem >> updateData [
	self children do: #updateData
]
