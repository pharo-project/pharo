"
A presenter for grouping menu items.
A group is always part of a menu (See `SpMenuPresenter`), and it groups items (See `SpMenuItemPresenter`).
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

{ #category : #documentation }
SpMenuGroupPresenter class >> documentExampleCodeSelector [

	^ #exampleMenuGroup
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

{ #category : #TOREMOVE }
SpMenuGroupPresenter >> autoRefresh [
	^ autoRefresh
]

{ #category : #TOREMOVE }
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
	"Answer when the presenter can take keyboard focus."

	^ false
]

{ #category : #TOREMOVE }
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
	"Answer true if the group is empty"

	^ items isEmpty
]

{ #category : #api }
SpMenuGroupPresenter >> menuItems [

	^ items value
]

{ #category : #private }
SpMenuGroupPresenter >> rootMenu [

	^ self owner rootMenu
]
