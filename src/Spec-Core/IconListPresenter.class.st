"
An IconListPresenter is a spec presenter for icon list.
See AbstractWidgetPresenter

self example

You can also have a look at ExampleListPresenter >> exampleIconList for another example.

I provide the following variables icons and its accessors, it is a block that convert an item in the icon to display near the item.

todo
- getIconFor:
"
Class {
	#name : #IconListPresenter,
	#superclass : #ColumnedListPresenter,
	#instVars : [
		'iconHolder',
		'iconColumn',
		'descriptionColumn',
		'displayBlock'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #example }
IconListPresenter class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		icons: [ :e | example iconNamed: #forwardIcon ];
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
]

{ #category : #accessing }
IconListPresenter >> displayBlock [
	^ displayBlock
]

{ #category : #accessing }
IconListPresenter >> displayBlock: anObject [
	displayBlock := anObject
]

{ #category : #initialization }
IconListPresenter >> displayValueOf: anObject [

	^ displayBlock value: anObject
]

{ #category : #api }
IconListPresenter >> getIconFor: anItem [

	^ iconHolder value cull: anItem cull: self
]

{ #category : #api }
IconListPresenter >> icons [
	"<api: #inspect>"
	"Return the block which takes an item as argument and returns the icon to display in the list"
	
	^ iconHolder value
]

{ #category : #api }
IconListPresenter >> icons: aBlock [

	iconHolder value: aBlock
]

{ #category : #initialization }
IconListPresenter >> initialize [

	super initialize.
	displayBlock := #yourself.
	iconHolder := [:item | nil ] asValueHolder.
	self hideColumnHeaders.
	self
		addColumn: (iconColumn := (ImageTableColumn title: 'Icon' evaluated: [:item | 
			self getIconFor: item ]));
		addColumn: (descriptionColumn := (StringTableColumn title: 'Description' evaluated: [ :item |
			self displayValueOf: item ]))
]

{ #category : #compatibility }
IconListPresenter >> updateList [
	self adapter widget refresh.
	self unselectAll
]

{ #category : #'api-events' }
IconListPresenter >> whenIconsChanged: aBlock [ 
	
	iconHolder whenChangedDo: aBlock 


]
