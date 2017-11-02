"
A ButtonPresenter is an applicative presenter which handle a basic button.
See AbstractWidgetPresenter

self example

You can also have a look at CustomerStatisfaction for another example.

I provide the following variables and their accessors
- action is the action performed when I am clicked.
- icon is the icon displayed on me.
- label is the text displayed on me.
- state is a boolean representing if I am highlighted, it is false by default and it is reversed while clicking.

todo
- askBeforeChanging
- font
- menu
- shortcut
"
Class {
	#name : #ButtonPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'actionHolder',
		'labelHolder',
		'fontHolder',
		'stateHolder',
		'actionPerformedHolder',
		'askBeforeChangingHolder',
		'iconHolder',
		'menuHolder',
		'shortcutHolder'
	],
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
ButtonPresenter class >> adapterName [

	^ #ButtonAdapter
]

{ #category : #specs }
ButtonPresenter class >> defaultSpec [
	<spec>
	
	^ #(ButtonAdapter
		adapt: #(model))
]

{ #category : #example }
ButtonPresenter class >> example [
	^ self new
		openWithSpec;
		label: 'Click me'
]

{ #category : #specs }
ButtonPresenter class >> title [

	^ 'Button'
]

{ #category : #api }
ButtonPresenter >> action [
	"<api: #inspect>"
	"get the block performed when the button is clicked"

	^ actionHolder value
]

{ #category : #api }
ButtonPresenter >> action: aBlock [
	"set the block performed when the button is clicked"
	"<api: #block getter: #getAction registration: #whenActionChangedDo:>"

	actionHolder value: aBlock
]

{ #category : #private }
ButtonPresenter >> addShortcutTipFor: aString [

	^ self 
		addShortcutTipFor: self shortcutCharacter
		on: aString
]

{ #category : #private }
ButtonPresenter >> addShortcutTipFor: aCharacter on: aString [
	| index text |
	
	aString isNil ifTrue: [ ^ nil ].
	text := aString asText.
	aCharacter ifNil: [ ^ text ].
	index := aString asLowercase indexOf: aCharacter asLowercase.
	index isZero ifTrue: [ ^ text ].
	
	^ text
		addAttribute: (TextEmphasis underlined) 
		from: index
		to: index;
		yourself
]

{ #category : #api }
ButtonPresenter >> askBeforeChanging [

	^ askBeforeChangingHolder value
]

{ #category : #api }
ButtonPresenter >> askBeforeChanging: aBoolean [

	askBeforeChangingHolder value: aBoolean
]

{ #category : #private-focus }
ButtonPresenter >> ensureKeyBindingsFor: widget [

	super ensureKeyBindingsFor: widget.
	self shortcut ifNotNil: [ :s | self registerShortcut: s ]
]

{ #category : #api }
ButtonPresenter >> font [

	^ fontHolder value
]

{ #category : #api }
ButtonPresenter >> font: aFont [

	fontHolder value: aFont
]

{ #category : #api }
ButtonPresenter >> getAction [
	"<api: #inspect>"
	"get the block performed when the button is clicked"

	^ actionHolder value
]

{ #category : #api }
ButtonPresenter >> icon [

	^ iconHolder value
]

{ #category : #api }
ButtonPresenter >> icon: anIcon [

	iconHolder value: anIcon
]

{ #category : #api }
ButtonPresenter >> iconName: aSymbol [

	iconHolder value: (self iconNamed: aSymbol)
]

{ #category : #initialization }
ButtonPresenter >> initialize [

	super initialize.

	actionHolder := [] asValueHolder.
	labelHolder := '' asValueHolder.
	fontHolder := nil asValueHolder.
	iconHolder := nil asValueHolder.
	stateHolder := false asValueHolder.
	actionPerformedHolder := nil asValueHolder.
	askBeforeChangingHolder := false asValueHolder.
	shortcutHolder := nil asValueHolder.
	
	labelHolder whenChangedDo: [ self changed: #label ].
	fontHolder whenChangedDo: [ self changed: #font ].
	iconHolder whenChangedDo: [ self changed: #label ].
	stateHolder whenChangedDo: [ self changed: #state ].
	askBeforeChangingHolder whenChangedDo: [:newValue | self changed: { #askBeforeChanging: . newValue} ].
	shortcutHolder whenChangedDo: [ :newShortcut :oldShortcut |
		self unregisterShortcut: oldShortcut.
		self registerShortcut: newShortcut.
		self changed: #label ].
	
	menuHolder := MenuPresenter new asValueHolder.	
	menuHolder whenChangedDo: [ :aMenuModel :oldMenu | 
		(oldMenu isNil or: [ oldMenu isBlock ]) ifFalse: [ oldMenu neglect: self ].
		(aMenuModel isNil or: [ aMenuModel isBlock ]) ifFalse: [ aMenuModel applyTo: self ] ].
		
	self bindKeyCombination: Character space toAction: [ self action ].
]

{ #category : #morphic }
ButtonPresenter >> label [
	"<api: #inspect>"
	
 	^ self addShortcutTipFor: labelHolder value
]

{ #category : #api }
ButtonPresenter >> label: aStringOrImageMorph [
	"<api: #string getter: #label registration: #whenLabelChangedDo:>"

	labelHolder value: aStringOrImageMorph
]

{ #category : #api }
ButtonPresenter >> menu [

	^ menuHolder value
]

{ #category : #api }
ButtonPresenter >> menu: aMenu [

	menuHolder value: aMenu
]

{ #category : #morphic }
ButtonPresenter >> performAction [

	actionHolder value value.
	" Here I set a dummy value just to make the holder raise an event "
	actionPerformedHolder value: nil.
]

{ #category : #private }
ButtonPresenter >> registerShortcut: newShortcut [
	| receiver |
	receiver := self window.
	(receiver isNil or: [ newShortcut isNil ]) ifTrue: [ ^ self ].

	receiver model
		bindKeyCombination: newShortcut 
		toAction: [ self performAction ]
]

{ #category : #api }
ButtonPresenter >> shortcut [

	^ shortcutHolder value
]

{ #category : #api }
ButtonPresenter >> shortcut: aShortcut [

	shortcutHolder value: aShortcut
]

{ #category : #private }
ButtonPresenter >> shortcutCharacter [

	^ self shortcut 
		ifNil: [ nil ]
		ifNotNil: [ :s | s currentCharacter ]
]

{ #category : #morphic }
ButtonPresenter >> state [
	"<api: #inspect>"
	^ stateHolder value
]

{ #category : #api }
ButtonPresenter >> state: aBoolean [
	"<api: #boolean getter: #state registration: #whenStateChangedDo:>"
	"set if the button is highlighted"
	
	^ stateHolder value: aBoolean
]

{ #category : #private }
ButtonPresenter >> unregisterShortcut: oldShortcut [
	| receiver |
	receiver := self window.
	(receiver isNil or: [ oldShortcut isNil ]) ifTrue: [ ^ self ].

	receiver model
		removeKeyCombination: oldShortcut 
]

{ #category : #api-events }
ButtonPresenter >> whenActionChangedDo: aBlock [ 

	actionHolder whenChangedDo: aBlock
]

{ #category : #api-events }
ButtonPresenter >> whenActionPerformedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	actionPerformedHolder whenChangedDo: aBlock
]

{ #category : #api-events }
ButtonPresenter >> whenFontChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	fontHolder whenChangedDo: aBlock
]

{ #category : #api-events }
ButtonPresenter >> whenLabelChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	labelHolder whenChangedDo: aBlock
]

{ #category : #api-events }
ButtonPresenter >> whenStateChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	stateHolder whenChangedDo: aBlock
]
