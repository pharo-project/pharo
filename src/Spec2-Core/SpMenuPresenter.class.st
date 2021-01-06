"
A presenter to describe a menu.
A menu presenter contains groups of menu items (having one unique group by default, to simplify building simple menus).
A menu presenter is included in presenters that implement the `#contextMenu:` (See below) method, but it can also be invoked by calling `SpMenuPresenter>>#openWithSpecAt:`
"
Class {
	#name : #SpMenuPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#groups',
		'#title => SpObservableSlot',
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

{ #category : #documentation }
SpMenuPresenter class >> addDocumentExtraSections: aBuilder [

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Presenters implementing `#contextMenu:`' ] withLevel: 2.
	aBuilder unorderedListDuring: [ 
		(SpTContextMenu users sorted: #name ascending) do: [ :eachType | 
			aBuilder item: [
				aBuilder monospace: eachType name ] ] ]	
]

{ #category : #documentation }
SpMenuPresenter class >> documentExampleCodeSelector [

	^ #exampleShowAtPointer
]

{ #category : #documentation }
SpMenuPresenter class >> documentFactoryMethodSelector [

	^ #newMenu
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
	"Add a group of items. 
	 If you do not want a multiple-group menu, you can use `SpMenuPresenter>>#addItem:` instead."
	| group |

	group := self instantiate: SpMenuGroupPresenter.
	aBlock value: group.
	self addMenuGroup: group
]

{ #category : #api }
SpMenuPresenter >> addItem: aBlock [
	"Add an item to the menu. When the menu group has not been specified previously, add the 
	 item in a default group.
	 For adding multiple groups, see `SpMenuPresenter>>#addGroup:`"

	self defaultGroup addItem: aBlock
]

{ #category : #private }
SpMenuPresenter >> addMenuGroup: aMenuGroup [

	aMenuGroup owner: self.
	groups add: aMenuGroup.
	self autoRefresh 
		ifNotNil: [ aMenuGroup autoRefresh: self autoRefresh ]
]

{ #category : #TOREMOVE }
SpMenuPresenter >> applyTo: aModel [

	aModel applyMenuModel: self
]

{ #category : #TOREMOVE }
SpMenuPresenter >> autoRefresh [
	^ autoRefresh
]

{ #category : #TOREMOVE }
SpMenuPresenter >> autoRefresh: aBoolean [
	autoRefresh := aBoolean
]

{ #category : #'api-building' }
SpMenuPresenter >> buildWithSpecAsPopup [

	^ self buildWithSpec
]

{ #category : #testing }
SpMenuPresenter >> canTakeKeyboardFocus [

	^ false
]

{ #category : #accessing }
SpMenuPresenter >> defaultGroup [

	^ defaultGroup ifNil: [ 
		defaultGroup := self instantiate: SpMenuGroupPresenter.
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

{ #category : #TOREMOVE }
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

{ #category : #localization }
SpMenuPresenter >> localeChanged [

	super localeChanged.
	self forceUpdateSlot: #title
	
]

{ #category : #api }
SpMenuPresenter >> menuGroups [
	^ groups
]

{ #category : #TOREMOVE }
SpMenuPresenter >> neglect: aModel [

	aModel neglectMenuModel: self
]

{ #category : #api }
SpMenuPresenter >> openWithSpecAt: aPosition [

	self buildWithSpecAsPopup.
	self withAdapterDo: [ :anAdapter | anAdapter openAt: aPosition ]
]

{ #category : #api }
SpMenuPresenter >> openWithSpecAtPointer [

	self buildWithSpecAsPopup.
	self withAdapterDo: [ :anAdapter | anAdapter openAtPointer ]
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
