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
	#name : #SpMenuGroupPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#items',
		'#autoRefresh => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpMenuGroupPresenter class >> adapterName [

	^ #MenuGroupAdapter
]

{ #category : #private }
SpMenuGroupPresenter >> add: aName target: targetObject selector: aSelector [
	
	self flag: #TODO. "This is for compatibility with the old menu builder and needs to be removed!"
	self addItem: [ :item | 
		item 
			name: aName; 
			action: [ targetObject perform: aSelector ] ].
	^ self menuItems last
]

{ #category : #api }
SpMenuGroupPresenter >> addItem: aBlock [
	| item |
	
	item := self instantiate: SpMenuItemPresenter.
	aBlock value: item.
	self addMenuItem: item
]

{ #category : #private }
SpMenuGroupPresenter >> addMenuItem: aMenuItem [

	aMenuItem owner: self.
	items add: aMenuItem.
	self autoRefresh 
		ifNotNil: [ aMenuItem autoRefresh: self autoRefresh ]
]

{ #category : #api }
SpMenuGroupPresenter >> autoRefresh [
	^ autoRefresh
]

{ #category : #api }
SpMenuGroupPresenter >> autoRefresh: aBoolean [
	autoRefresh := aBoolean
]

{ #category : #private }
SpMenuGroupPresenter >> buildWithSpecLayout: aSpecLayout [
	"Build the widget using the spec name provided as argument"
	| widget|
	adapter := self basicBuildAdapterWithSpecLayout: aSpecLayout.
	widget := adapter widget.
	self announce: (SpWidgetBuilt model: self widget: widget).
	^ widget
]

{ #category : #testing }
SpMenuGroupPresenter >> canTakeKeyboardFocus [

	^ false
]

{ #category : #'api-building' }
SpMenuGroupPresenter >> fromSpec: aSpec [
	self addItem: [ :item | item fromSpec: aSpec ]
]

{ #category : #initialization }
SpMenuGroupPresenter >> initialize [
	super initialize.

	items := OrderedCollection new asValueHolder.
	self property: #autoRefresh whenChangedDo: [ :aBoolean | self menuItems do: [ :each | each autoRefresh: aBoolean ] ]
]

{ #category : #testing }
SpMenuGroupPresenter >> isEmpty [

	^ items isEmpty
]

{ #category : #api }
SpMenuGroupPresenter >> menuItems [

	^ items value
]

{ #category : #accessing }
SpMenuGroupPresenter >> rootMenu [

	^ self owner rootMenu
]
