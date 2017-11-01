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
	#superclass : #ListPresenter,
	#instVars : [
		'iconHolder'
	],
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
IconListPresenter class >> adapterName [

	^ #IconListAdapter
]

{ #category : #specs }
IconListPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(IconListAdapter
		adapt: #(model))
]

{ #category : #example }
IconListPresenter class >> example [
	| example |
	example := self new.
	example
		icons: [ :e | example iconNamed: #forwardIcon ];
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
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
	"<api: #block getter: #icon registration: #whenIconsChanged: >"
	"Set a block which takes an item as argument and returns the icon to display in the list"
	iconHolder value: aBlock
]

{ #category : #initialization }
IconListPresenter >> initialize [

	super initialize.

	iconHolder := [:item | nil ] asValueHolder.
]

{ #category : #api-events }
IconListPresenter >> whenIconsChanged: aBlock [ 
	
	iconHolder whenChangedDo: aBlock 


]
