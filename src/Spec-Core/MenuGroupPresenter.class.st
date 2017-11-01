"
I am a group of menu items.

I am part of a menu (MenuPresenter), and groups items (MenuItemPresenter) by meaning.

You can also have a look at ApplicationWithToolbar for a full example of how to use them.

I provide the following methods
- addItem: use the block in parameter to initialize a new item, then add it in my collection.
- addMenuItem: add the item in parameter in my collection.
- isEmpty return a boolean representing if my collection is empty.
- menuItems getter of my collection.


todo
- autoRefresh
- buildWithSpecLayout:
- fromSpec:
"
Class {
	#name : #MenuGroupPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'items',
		'autoRefresh'
	],
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
MenuGroupPresenter class >> adapterName [

	^ #MenuGroupAdapter
]

{ #category : #specs }
MenuGroupPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(MenuGroupAdapter
		adapt: #(model))
]

{ #category : #api }
MenuGroupPresenter >> addItem: aBlock [
	| item |
	
	item := MenuItemPresenter new.
	aBlock value: item.
	self addMenuItem: item
]

{ #category : #private }
MenuGroupPresenter >> addMenuItem: aMenuItem [

	items add: aMenuItem.
	self autoRefresh 
		ifNotNil: [ aMenuItem autoRefresh: self autoRefresh ]
]

{ #category : #api }
MenuGroupPresenter >> autoRefresh [

	^ autoRefresh value
]

{ #category : #api }
MenuGroupPresenter >> autoRefresh: aBoolean [

	autoRefresh value: aBoolean
]

{ #category : #private }
MenuGroupPresenter >> buildWithSpecLayout: aSpecLayout [
	"Build the widget using the spec name provided as argument"
	| adapter widget|
	adapter := self adapterFrom: aSpecLayout model: self.
	widget := adapter asWidget.
	self announce: (WidgetBuilt model: self widget: widget).
	^ widget
]

{ #category : #api-building }
MenuGroupPresenter >> fromSpec: aSpec [

	aSpec addToMenuGroupModel: self
]

{ #category : #initialization }
MenuGroupPresenter >> initialize [
	super initialize.
	items := OrderedCollection new asValueHolder.
	autoRefresh := nil asValueHolder.
	autoRefresh whenChangedDo: [ :aBoolean | self menuItems do: [ :each | each autoRefresh: aBoolean ] ]
]

{ #category : #testing }
MenuGroupPresenter >> isEmpty [

	^ items isEmpty
]

{ #category : #api }
MenuGroupPresenter >> menuItems [

	^ items value
]
