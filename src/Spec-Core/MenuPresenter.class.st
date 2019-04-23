"
I am a simple presenter describing a menu.
See AbstractWidgetPresenter

I only contains a list of menu groups (MenuGroupPresenter). Each group is separated by a splitter.

I provide the following variables and their accessors
- addGroup: use the block in parameter to initialize a new group, then add it in my collection.
- addMenuGroup: add the group in parameter in my collection.
- menuGroups getter of my collection.


todo
- addAllFromPragma:target:
- applyTo: activate the shortcut of the items of my groups.
- autoRefresh
- buildWithSpecAsPopup
- fromSpec:
- icon
- neglect:
- openWithSpecAt:
- printOn:
- title , addTitle:

"
Class {
	#name : #MenuPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'groups',
		'title',
		'icon',
		'autoRefresh',
		'shortcutGroup',
		'defaultGroup'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
MenuPresenter class >> adapterName [

	^ #MenuAdapter
]

{ #category : #specs }
MenuPresenter class >> popup [
	<spec>
	
	^ #(MenuAdapter
		adaptAsPopup: #(presenter))
]

{ #category : #'api-building' }
MenuPresenter >> addAllFromPragma: pragma target: target [

	self fromSpec: (PragmaMenuBuilder 
		pragmaKeyword: pragma
		model: target) menuSpec
]

{ #category : #api }
MenuPresenter >> addGroup: aBlock [
	"Create a group of items in case you want a multi group menu.
	In case of a single group menu you can use #addItem: directly."

	| group |
	group := MenuGroupPresenter new.
	aBlock value: group.
	self addMenuGroup: group
]

{ #category : #api }
MenuPresenter >> addItem: aBlock [
	"Add an item to the menu. When the menu group has not been specified previously, add the item in a default group.
	For managing yourself groups, check #addGroup:"

	self defaultGroup addItem: aBlock
]

{ #category : #private }
MenuPresenter >> addMenuGroup: aMenuGroup [

	aMenuGroup owner: self.
	groups add: aMenuGroup.
	self autoRefresh 
		ifNotNil: [ aMenuGroup autoRefresh: self autoRefresh ]
]

{ #category : #adding }
MenuPresenter >> addTitle: aString [
	title value: aString
]

{ #category : #api }
MenuPresenter >> applyTo: aModel [

	aModel applyMenuModel: self
]

{ #category : #api }
MenuPresenter >> autoRefresh [

	^ autoRefresh value
]

{ #category : #api }
MenuPresenter >> autoRefresh: aBoolean [

	autoRefresh value: aBoolean
]

{ #category : #api }
MenuPresenter >> buildWithSpecAsPopup [

	^ self buildWithSpec: #popup
]

{ #category : #accessing }
MenuPresenter >> defaultGroup [
	^ defaultGroup
		ifNil: [ defaultGroup := MenuGroupPresenter new.
			self addMenuGroup: defaultGroup.
			defaultGroup ]
]

{ #category : #'api-building' }
MenuPresenter >> fromSpec: aSpec [

	aSpec addToMenuModel: self
]

{ #category : #api }
MenuPresenter >> icon [

	^ icon value
]

{ #category : #api }
MenuPresenter >> icon: anIcon [

	icon value: anIcon
]

{ #category : #api }
MenuPresenter >> iconName: aSymbol [

	icon value: (self iconNamed: aSymbol)
]

{ #category : #initialization }
MenuPresenter >> initialize [

	super initialize.
	groups := OrderedCollection new asValueHolder.
	title := nil asValueHolder.
	icon := nil asValueHolder.
	autoRefresh := nil asValueHolder.
	autoRefresh whenChangedDo: [ :aBoolean | self menuGroups do: [ :each | each autoRefresh: aBoolean ] ]
]

{ #category : #testing }
MenuPresenter >> isMenuPresenter [

	^ true
]

{ #category : #api }
MenuPresenter >> menuGroups [

	^ groups value
]

{ #category : #api }
MenuPresenter >> neglect: aModel [

	aModel neglectMenuModel: self
]

{ #category : #api }
MenuPresenter >> openWithSpecAt: aPosition [

	self buildWithSpecAsPopup.
	self changed: #openAt: with: { aPosition }
]

{ #category : #printing }
MenuPresenter >> printOn: aStream [
	super printOn: aStream.
	self title value
		ifNotNil: [ :t | 
			aStream
				nextPutAll: ' ''';
				nextPutAll: t;
				nextPutAll: '''' ]
]

{ #category : #accessing }
MenuPresenter >> rootMenu [

	^ self owner isMenuPresenter 
		ifTrue: [ self owner rootMenu ]
		ifFalse: [ self ]

]

{ #category : #accessing }
MenuPresenter >> shortcutGroup [
	"Some platforms use acceleration groups to store shortcut keys. 
	 if that's the case, we use this property to access it later (and store 
	 in windows, for example)"

	^ shortcutGroup
]

{ #category : #accessing }
MenuPresenter >> shortcutGroup: aKeyGroup [
	
	shortcutGroup := aKeyGroup
]

{ #category : #api }
MenuPresenter >> title [

	^ title value
]

{ #category : #api }
MenuPresenter >> title: aString [

	title value: aString
]
