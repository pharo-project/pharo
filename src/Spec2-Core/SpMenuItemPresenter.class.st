"
A presenter to define a menu item. 
A menu item needs to be included into a group (See `SpMenuGroupPresenter`) and through it a context menu (See `SpMenuPresenter`), or a menu bar (See `SpMenuBarPresenter`).

A menu item can have: an action (See `SpMenuItemPresenter>>#action:`) **OR** it can include a submenu (See `SpMenuItemPresenter>>#subMenu:`), but it cannot have both properties at a time.

Tipically, you do not call directly `SpMenuItemPresenter`. Instead, you use the builder method provided when building a `SpMenuPresenter` (See example below).
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

{ #category : #documentation }
SpMenuItemPresenter class >> documentExampleCodeSelector [

	^ #exampleShowAtPointer
]

{ #category : #api }
SpMenuItemPresenter >> action [
	"Answer the action to be executed when the menu item is selected."

	^ action
]

{ #category : #api }
SpMenuItemPresenter >> action: aBlock [
	"Set the action to be executed when the menu item is selected.
	 `aBlock` receives zero arguments."

	action := aBlock
]

{ #category : #TOREMOVE }
SpMenuItemPresenter >> autoRefresh [
	^ autoRefresh
]

{ #category : #TOREMOVE }
SpMenuItemPresenter >> autoRefresh: aBlock [
	autoRefresh := aBlock
]

{ #category : #api }
SpMenuItemPresenter >> description [
	"Answer the description for the menu item."

	^ description
]

{ #category : #api }
SpMenuItemPresenter >> description: aString [
	"Set a description for the menu item. In some platforms (like Morphic), it can be shown as a 
	 tooltip text."

	description := aString
]

{ #category : #TOREMOVE }
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
	"Answer the icon to be shown with the menu item."

	^ icon
]

{ #category : #api }
SpMenuItemPresenter >> icon: anIcon [
	"Set an icon (an instance of `Form`) to be shown with the menu item."

	icon := anIcon
]

{ #category : #TOREMOVE }
SpMenuItemPresenter >> iconName: aSymbol [
	"This will need to be removed because #iconNamed: will be depending on application, hence it 
	 does not has sense."

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

{ #category : #TOREMOVE }
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
	"Answer the name of the menu item. This will be displayed as label."

	^ name
]

{ #category : #api }
SpMenuItemPresenter >> name: aString [
	"The label of the menu item. 
	 In certain backends (Gtk), we allow this label to contain mnemonics using an underscore ($_)
	 before the mnemonic letter: 
	 ```
	 aMenuItemPresenter name: '_File'.
	 ```"

	name := aString
]

{ #category : #private }
SpMenuItemPresenter >> performMenuActionWith: aMenuItem [
	| en |

	en := self isEnabled.
	en isBlock ifTrue: [ en := en value ].
	en ifFalse: [ ^ self ].

	self action cull: aMenuItem
]

{ #category : #private }
SpMenuItemPresenter >> rootMenu [ 
	
	^ self owner rootMenu
]

{ #category : #api }
SpMenuItemPresenter >> shortcut [
	^ shortcut
]

{ #category : #api }
SpMenuItemPresenter >> shortcut: aShortcut [
	"Set a shortcut (an instance of `KMKeyCombination`) that be shown with the menu item. 
	 **IMPORTANT:** Since most menus are added dinamically to its presenters (as part of a 
	 context menu), the shortcut defined in the menu is **NOT** automatically added to the 
	 window. User needs to declare them using `SpPresenter>>#bindKeyCombination:toAction:` 
	 or `SpPresenter>>#contextKeyBindings:`"
	
	shortcut := aShortcut
]

{ #category : #TOREMOVE }
SpMenuItemPresenter >> state [
	^ state
]

{ #category : #TOREMOVE }
SpMenuItemPresenter >> state: aBoolean [

	self flag: #REVIEW. "Maybe not removed but implemented correctly?"
	state := aBoolean
]

{ #category : #api }
SpMenuItemPresenter >> subMenu [
	"Answer the submenu that will be displayed with this menu if any, or nil."

	^ subMenu
]

{ #category : #api }
SpMenuItemPresenter >> subMenu: aMenuPresenter [
	"Set the submenu that will be displayed with this menu.
	 If a submenu is shown, defined action (See `SpMenuItemPresenter>>#action:`) will be ignored."


	aMenuPresenter ifNotNil: [ aMenuPresenter owner: self ].
	subMenu := aMenuPresenter
]

{ #category : #'api-events' }
SpMenuItemPresenter >> whenDescriptionChangedDo: aBlock [ 
	"Inform when description has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #description whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpMenuItemPresenter >> whenIconChangedDo: aBlock [ 
	"Inform when icon has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #icon whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpMenuItemPresenter >> whenNameChangedDo: aBlock [ 
	"Inform when name  has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #name whenChangedDo: aBlock
]
