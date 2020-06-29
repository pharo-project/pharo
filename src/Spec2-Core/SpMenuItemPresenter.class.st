"
I represent a menu item.

You can also have a look at ApplicationWithToolbar for a full example of how to use them.

I collaborate with MenuGroupPresenter to manage me.

I provide the following variables and their accessors
- name is the text I display.
- icon is the text I display.
- description is the text I display in my tooltip.
- action is the action to perform when I am clicked.
- subMenu is the submenu to display when I am clicked.

Usually I do NOT have an action AND a submenu.

todo
- autoRefresh
- enabled
- fromSpec:
- performMenuActionWith:
- shortcut is ..., the shortcut is displayed near my name.
- state
"
Class {
	#name : #SpMenuItemPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#name => SpObservableSlot',
		'#icon => SpObservableSlot',
		'#description => SpObservableSlot',
		'#action',
		'#shortcut',
		'#subMenu',
		'#state',
		'#autoRefresh'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpMenuItemPresenter class >> adapterName [

	^ #MenuItemAdapter
]

{ #category : #accessing }
SpMenuItemPresenter >> action [
	^ action
]

{ #category : #accessing }
SpMenuItemPresenter >> action: aBlock [
	action := aBlock
]

{ #category : #accessing }
SpMenuItemPresenter >> autoRefresh [
	^ autoRefresh
]

{ #category : #accessing }
SpMenuItemPresenter >> autoRefresh: aBlock [
	autoRefresh := aBlock
]

{ #category : #api }
SpMenuItemPresenter >> description [
	^ description
]

{ #category : #api }
SpMenuItemPresenter >> description: aString [
	description := aString
]

{ #category : #'api-building' }
SpMenuItemPresenter >> fromSpec: aSpec [
	| en newSubMenu |
	en := aSpec enabledBlock ifNil: [ aSpec enabled ].
	newSubMenu := aSpec subMenu ifNotNil: [ :sub | SpMenuPresenter new fromSpec: sub ].
	self
		name: aSpec label;
		state: aSpec checked;
		enabled: en;
		icon: aSpec icon;
		shortcut: aSpec keyText;
		subMenu: newSubMenu;
		action: aSpec action
]

{ #category : #api }
SpMenuItemPresenter >> icon [
	^ icon
]

{ #category : #api }
SpMenuItemPresenter >> icon: anIcon [
	icon := anIcon
]

{ #category : #api }
SpMenuItemPresenter >> iconName: aSymbol [
	self icon: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpMenuItemPresenter >> initialize [
	super initialize.

	action := [  ].
	name := ''.
	autoRefresh := false
]

{ #category : #testing }
SpMenuItemPresenter >> isMenuPresenter [

	^ true
]

{ #category : #accessing }
SpMenuItemPresenter >> keyText: aString [

	self flag: #TODO. "This is for backward compatibility and needs to be removed!"
	self shortcut: aString first meta
]

{ #category : #localization }
SpMenuItemPresenter >> localeChanged [

	super localeChanged.
	self forceUpdateSlot: #name
	
]

{ #category : #api }
SpMenuItemPresenter >> name [
	^ name
]

{ #category : #api }
SpMenuItemPresenter >> name: aString [
	"The label of the menu item. 
	 In certain backends (Gtk), we allow this label to contain mnemonics using an underscore ($_)
	 before the mnemonic letter: 
	[[[language=Smalltalk
	aMenuItemPresenter name: '_File'.
	]]]"

	name := aString
]

{ #category : #api }
SpMenuItemPresenter >> performMenuActionWith: aMenuItem [
	| en |

	en := self isEnabled.
	en isBlock ifTrue: [ en := en value ].
	en ifFalse: [ ^ self ].

	self action cull: aMenuItem
]

{ #category : #accessing }
SpMenuItemPresenter >> rootMenu [ 
	
	^ self owner rootMenu
]

{ #category : #api }
SpMenuItemPresenter >> shortcut [
	^ shortcut
]

{ #category : #api }
SpMenuItemPresenter >> shortcut: aShortcut [
	shortcut := aShortcut
]

{ #category : #accessing }
SpMenuItemPresenter >> state [
	^ state
]

{ #category : #accessing }
SpMenuItemPresenter >> state: aBoolean [
	state := aBoolean
]

{ #category : #api }
SpMenuItemPresenter >> subMenu [
	^ subMenu
]

{ #category : #api }
SpMenuItemPresenter >> subMenu: aMenuPresenter [
	aMenuPresenter ifNotNil: [ aMenuPresenter owner: self ].
	subMenu := aMenuPresenter
]

{ #category : #'api-events' }
SpMenuItemPresenter >> whenDescriptionChangedDo: aBlock [ 

	self property: #description whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpMenuItemPresenter >> whenIconChangedDo: aBlock [ 

	self property: #icon whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpMenuItemPresenter >> whenNameChangedDo: aBlock [ 

	self property: #name whenChangedDo: aBlock
]
