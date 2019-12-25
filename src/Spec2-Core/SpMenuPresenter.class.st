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
	#name : #SpMenuPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#groups',
		'#title',
		'#icon',
		'#autoRefresh => SpObservableSlot',
		'#shortcutGroup',
		'#defaultGroup'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpMenuPresenter class >> adapterName [

	^ #MenuAdapter
]

{ #category : #specs }
SpMenuPresenter class >> popup [
	<spec>
	
	^ #(MenuAdapter
		adaptAsPopup: #(presenter))
]

{ #category : #'api-building' }
SpMenuPresenter >> addAllFromPragma: pragma target: target [

	self fromSpec: (PragmaMenuBuilder 
		pragmaKeyword: pragma
		model: target) menuSpec
]

{ #category : #api }
SpMenuPresenter >> addGroup: aBlock [
	"Create a group of items in case you want a multi group menu.
	In case of a single group menu you can use #addItem: directly."

	| group |
	group := self instantiate: SpMenuGroupPresenter.
	aBlock value: group.
	self addMenuGroup: group
]

{ #category : #api }
SpMenuPresenter >> addItem: aBlock [
	"Add an item to the menu. When the menu group has not been specified previously, add the item in a default group.
	For managing yourself groups, check #addGroup:"

	self defaultGroup addItem: aBlock
]

{ #category : #private }
SpMenuPresenter >> addMenuGroup: aMenuGroup [

	aMenuGroup owner: self.
	groups add: aMenuGroup.
	self autoRefresh 
		ifNotNil: [ aMenuGroup autoRefresh: self autoRefresh ]
]

{ #category : #api }
SpMenuPresenter >> applyTo: aModel [

	aModel applyMenuModel: self
]

{ #category : #api }
SpMenuPresenter >> autoRefresh [
	^ autoRefresh
]

{ #category : #api }
SpMenuPresenter >> autoRefresh: aBoolean [
	autoRefresh := aBoolean
]

{ #category : #api }
SpMenuPresenter >> buildWithSpecAsPopup [

	^ self buildWithSpec: #popup
]

{ #category : #testing }
SpMenuPresenter >> canTakeKeyboardFocus [

	^ false
]

{ #category : #accessing }
SpMenuPresenter >> defaultGroup [
	^ defaultGroup
		ifNil: [ defaultGroup := self instantiate: SpMenuGroupPresenter.
			self addMenuGroup: defaultGroup.
			defaultGroup ]
]

{ #category : #'api-building' }
SpMenuPresenter >> fromSpec: aSpec [
	| grps subgroup |
	
	grps := OrderedCollection new.
	aSpec label ifNotNil: [ :label | self title: label ]. "only change the title when a new one is specified"
	
	subgroup := OrderedCollection new.
	aSpec items do: [ :each | 
		subgroup add: each.
		each separator
			ifTrue: [ 
				grps add: subgroup copy.
				subgroup removeAll ] ].
	subgroup ifNotEmpty: [ grps add: subgroup copy ].
	
	grps do: [ :each | self 
		addGroup: [ :group | each do: [ :spec | 
			group addItem: [ :item | item fromSpec: spec ] ] ] ]
]

{ #category : #api }
SpMenuPresenter >> icon [
	^ icon
]

{ #category : #api }
SpMenuPresenter >> icon: anIcon [
	icon := anIcon
]

{ #category : #api }
SpMenuPresenter >> iconName: aSymbol [
	self icon: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpMenuPresenter >> initialize [
	super initialize.

	groups := OrderedCollection new.
	self property: #autoRefresh whenChangedDo: [ :aBoolean | self menuGroups do: [ :each | each autoRefresh: aBoolean ] ]
]

{ #category : #testing }
SpMenuPresenter >> isMenuPresenter [

	^ true
]

{ #category : #api }
SpMenuPresenter >> menuGroups [
	^ groups
]

{ #category : #api }
SpMenuPresenter >> neglect: aModel [

	aModel neglectMenuModel: self
]

{ #category : #api }
SpMenuPresenter >> openWithSpecAt: aPosition [

	self buildWithSpecAsPopup.
	self changed: #openAt: with: { aPosition }
]

{ #category : #printing }
SpMenuPresenter >> printOn: aStream [
	super printOn: aStream.
	self title value
		ifNotNil: [ :t | 
			aStream
				nextPutAll: ' ''';
				nextPutAll: t;
				nextPutAll: '''' ]
]

{ #category : #accessing }
SpMenuPresenter >> rootMenu [

	^ self owner isMenuPresenter 
		ifTrue: [ self owner rootMenu ]
		ifFalse: [ self ]

]

{ #category : #accessing }
SpMenuPresenter >> shortcutGroup [
	"Some platforms use acceleration groups to store shortcut keys. 
	 if that's the case, we use this property to access it later (and store 
	 in windows, for example)"

	^ shortcutGroup
]

{ #category : #accessing }
SpMenuPresenter >> shortcutGroup: aKeyGroup [
	
	shortcutGroup := aKeyGroup
]

{ #category : #api }
SpMenuPresenter >> title [
	^ title
]

{ #category : #api }
SpMenuPresenter >> title: aString [
	title := aString
]
