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
		'#action => SpObservableSlot',
		'#actionPerformed => SpObservableSlot',
		'#askBeforeChanging => SpObservableSlot',
		'#contextMenu => SpObservableSlot',
		'#font => SpObservableSlot',
		'#icon => SpObservableSlot',
		'#label => SpObservableSlot',
		'#shortcut => SpObservableSlot',
		'#state => SpObservableSlot'
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
	"get the block performed when the button is clicked"

	^ action
]

{ #category : #api }
SpButtonPresenter >> action: aBlock [
	"set the block performed when the button is clicked"

	action := aBlock
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
	^ askBeforeChanging
]

{ #category : #api }
SpButtonPresenter >> askBeforeChanging: aBoolean [
	askBeforeChanging := aBoolean
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
	^ contextMenu
]

{ #category : #api }
SpButtonPresenter >> contextMenu: aMenu [
	contextMenu := aMenu
]

{ #category : #'private-focus' }
SpButtonPresenter >> ensureKeyBindingsFor: widget [

	super ensureKeyBindingsFor: widget.
	self shortcut ifNotNil: [ :s | self registerShortcut: s ]
]

{ #category : #api }
SpButtonPresenter >> font [
	^ font
]

{ #category : #api }
SpButtonPresenter >> font: aFont [
	font := aFont
]

{ #category : #api }
SpButtonPresenter >> icon [
	^ icon
]

{ #category : #api }
SpButtonPresenter >> icon: anIcon [
	icon := anIcon
]

{ #category : #api }
SpButtonPresenter >> iconName: aSymbol [
	self icon: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpButtonPresenter >> initialize [
	super initialize.

	action := [  ].
	label := ''.
	state := false.
	askBeforeChanging := false.

	self whenLabelChangedDo: [ self changed: #label ].
	self whenFontChangedDo: [ self changed: #font ].
	self whenIconChangedDo: [ self changed: #label ].
	self whenStateChangedDo: [ self changed: #state ].
	self whenMenuChangedDo: [ self withAdapterDo: #updateMenu ].
	self property: #askBeforeChanging whenChangedDo: [ :newValue | self changed: {#askBeforeChanging: . newValue} ].
	self
		property: #shortcut
		whenChangedDo: [ :newShortcut :oldShortcut | 
			self unregisterShortcut: oldShortcut.
			self registerShortcut: newShortcut.
			self changed: #label ].

	self bindKeyCombination: Character space toAction: [ self action ].

	self color: nil
]

{ #category : #morphic }
SpButtonPresenter >> label [
	^ self addShortcutTipFor: label
]

{ #category : #api }
SpButtonPresenter >> label: aStringOrImageMorph [
	label := aStringOrImageMorph
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
	self action value.

	" Here I set a dummy value just to make the holder raise an event "
	actionPerformed := nil
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
	^ shortcut
]

{ #category : #api }
SpButtonPresenter >> shortcut: aShortcut [
	shortcut := aShortcut
]

{ #category : #private }
SpButtonPresenter >> shortcutCharacter [

	^ self shortcut 
		ifNil: [ nil ]
		ifNotNil: [ :s | s spCurrentCharacter ]
]

{ #category : #morphic }
SpButtonPresenter >> state [
	^ state
]

{ #category : #api }
SpButtonPresenter >> state: aBoolean [
	"set if the button is highlighted"

	state := aBoolean
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
	self property: #action whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenActionPerformedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	self property: #actionPerformed whenChangedDo: aBlock
]

{ #category : #'API - events' }
SpButtonPresenter >> whenActivatedDo: aBlockClosure [ 
	
	^ self whenActionPerformedDo: aBlockClosure 
]

{ #category : #'api-events' }
SpButtonPresenter >> whenFontChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	self property: #font whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenIconChangedDo: aBlock [
	self property: #icon whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenLabelChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	self property: #label whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenMenuChangedDo: aBlock [
 
	self property: #contextMenu whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenStateChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	self property: #state whenChangedDo: aBlock
]
