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
	#name : #SpButtonPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'actionHolder',
		'labelHolder',
		'fontHolder',
		'stateHolder',
		'actionPerformedHolder',
		'askBeforeChangingHolder',
		'iconHolder',
		'shortcutHolder',
		'contextMenuHolder'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpButtonPresenter class >> adapterName [

	^ #ButtonAdapter
]

{ #category : #specs }
SpButtonPresenter class >> title [

	^ 'Button'
]

{ #category : #api }
SpButtonPresenter >> action [
	"<api: #inspect>"
	"get the block performed when the button is clicked"

	^ actionHolder value
]

{ #category : #api }
SpButtonPresenter >> action: aBlock [
	"set the block performed when the button is clicked"
	"<api: #block getter: #getAction registration: #whenActionChangedDo:>"

	actionHolder value: aBlock
]

{ #category : #private }
SpButtonPresenter >> addShortcutTipFor: aString [

	^ self 
		addShortcutTipFor: self shortcutCharacter
		on: aString
]

{ #category : #private }
SpButtonPresenter >> addShortcutTipFor: aCharacter on: aString [
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
SpButtonPresenter >> askBeforeChanging [

	^ askBeforeChangingHolder value
]

{ #category : #api }
SpButtonPresenter >> askBeforeChanging: aBoolean [

	askBeforeChangingHolder value: aBoolean
]

{ #category : #simulating }
SpButtonPresenter >> click [

	"I emulate the click from the presenter point of view.
	Of course if the button is disabled, I do nothing, like in real life!!
	If you want to emulate other stuff use performAction"
	
	self isEnabled ifFalse: [ ^ self ].
	^ self performAction
]

{ #category : #api }
SpButtonPresenter >> contextMenu [
		
	^ contextMenuHolder value
]

{ #category : #api }
SpButtonPresenter >> contextMenu: aMenu [
		
	contextMenuHolder value: aMenu
]

{ #category : #'private-focus' }
SpButtonPresenter >> ensureKeyBindingsFor: widget [

	super ensureKeyBindingsFor: widget.
	self shortcut ifNotNil: [ :s | self registerShortcut: s ]
]

{ #category : #api }
SpButtonPresenter >> font [

	^ fontHolder value
]

{ #category : #api }
SpButtonPresenter >> font: aFont [

	fontHolder value: aFont
]

{ #category : #api }
SpButtonPresenter >> getAction [
	"<api: #inspect>"
	"get the block performed when the button is clicked"

	^ actionHolder value
]

{ #category : #api }
SpButtonPresenter >> icon [

	^ iconHolder value
]

{ #category : #api }
SpButtonPresenter >> icon: anIcon [

	iconHolder value: anIcon
]

{ #category : #api }
SpButtonPresenter >> iconName: aSymbol [

	iconHolder value: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpButtonPresenter >> initialize [

	super initialize.

	actionHolder := [] asValueHolder.
	labelHolder := '' asValueHolder.
	fontHolder := nil asValueHolder.
	iconHolder := nil asValueHolder.
	stateHolder := false asValueHolder.
	actionPerformedHolder := nil asValueHolder.
	askBeforeChangingHolder := false asValueHolder.
	shortcutHolder := nil asValueHolder.
	contextMenuHolder := nil asValueHolder.	
	
	labelHolder whenChangedDo: [ self changed: #label ].
	fontHolder whenChangedDo: [ self changed: #font ].
	iconHolder whenChangedDo: [ self changed: #label ].
	stateHolder whenChangedDo: [ self changed: #state ].
	contextMenuHolder whenChangedDo: [ self withAdapterDo: #updateMenu ].
	askBeforeChangingHolder whenChangedDo: [:newValue | 
		self changed: { #askBeforeChanging: . newValue} ].
	shortcutHolder whenChangedDo: [ :newShortcut :oldShortcut |
		self unregisterShortcut: oldShortcut.
		self registerShortcut: newShortcut.
		self changed: #label ].
	
	self bindKeyCombination: Character space toAction: [ self action ].
	
	self color: nil.
]

{ #category : #morphic }
SpButtonPresenter >> label [
	"<api: #inspect>"
	
 	^ self addShortcutTipFor: labelHolder value
]

{ #category : #api }
SpButtonPresenter >> label: aStringOrImageMorph [
	"<api: #string getter: #label registration: #whenLabelChangedDo:>"

	labelHolder value: aStringOrImageMorph
]

{ #category : #api }
SpButtonPresenter >> menu [

	self 
		deprecated: 'Use #contextMenu instead.' 
		transformWith: '`@receiver menu' 
						-> '`@receiver contextMenu'.

	^ self contextMenu
]

{ #category : #api }
SpButtonPresenter >> menu: aMenu [

	self 
		deprecated: 'Use #contextMenu instead.' 
		transformWith: '`@receiver menu: `@argument' 
						-> '`@receiver contextMenu: `@argument'.
		
	self contextMenu: aMenu
]

{ #category : #morphic }
SpButtonPresenter >> performAction [

	actionHolder value value.
	" Here I set a dummy value just to make the holder raise an event "
	actionPerformedHolder value: nil.
]

{ #category : #private }
SpButtonPresenter >> registerShortcut: newShortcut [
	| receiver |
	receiver := self window.
	(receiver isNil or: [ newShortcut isNil ]) ifTrue: [ ^ self ].
	
	receiver presenter
		bindKeyCombination: newShortcut
		toAction: [ self performAction ]
]

{ #category : #api }
SpButtonPresenter >> shortcut [

	^ shortcutHolder value
]

{ #category : #api }
SpButtonPresenter >> shortcut: aShortcut [

	shortcutHolder value: aShortcut
]

{ #category : #private }
SpButtonPresenter >> shortcutCharacter [

	^ self shortcut 
		ifNil: [ nil ]
		ifNotNil: [ :s | s spCurrentCharacter ]
]

{ #category : #morphic }
SpButtonPresenter >> state [
	"<api: #inspect>"
	^ stateHolder value
]

{ #category : #api }
SpButtonPresenter >> state: aBoolean [
	"<api: #boolean getter: #state registration: #whenStateChangedDo:>"
	"set if the button is highlighted"
	
	^ stateHolder value: aBoolean
]

{ #category : #private }
SpButtonPresenter >> unregisterShortcut: oldShortcut [
	| receiver |
	receiver := self window.
	(receiver isNil or: [ oldShortcut isNil ]) ifTrue: [ ^ self ].

	receiver model
		removeKeyCombination: oldShortcut 
]

{ #category : #'api-events' }
SpButtonPresenter >> whenActionChangedDo: aBlock [ 

	actionHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenActionPerformedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	actionPerformedHolder whenChangedDo: aBlock
]

{ #category : #'API - events' }
SpButtonPresenter >> whenActivatedDo: aBlockClosure [ 
	
	^ self whenActionPerformedDo: aBlockClosure 
]

{ #category : #'api-events' }
SpButtonPresenter >> whenFontChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	fontHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenIconChangedDo: aBlock [

	iconHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenLabelChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	labelHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenMenuChangedDo: aBlock [
 
	contextMenuHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenStateChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	stateHolder whenChangedDo: aBlock
]
