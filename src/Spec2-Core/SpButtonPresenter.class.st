"
A ButtonPresenter is an applicative presenter which handle a basic button.
See AbstractWidgetPresenter

self example

I provide the following variables and their accessors
- action is the action performed when I am clicked.
- icon is the icon displayed on me.
- label is the text displayed on me.
- state is a boolean representing if I am highlighted, it is false by default and it is reversed while clicking.
"
Class {
	#name : #SpButtonPresenter,
	#superclass : #SpAbstractButtonPresenter,
	#traits : 'SpTContextMenu',
	#classTraits : 'SpTContextMenu classTrait',
	#instVars : [
		'#action => SpObservableSlot',
		'#actionPerformed => SpObservableSlot',
		'#askBeforeChanging => SpObservableSlot',
		'#font => SpObservableSlot',
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

{ #category : #initialization }
SpButtonPresenter >> initialize [
	super initialize.

	action := [  ].
	state := false.
	askBeforeChanging := false.

	self whenFontChangedDo: [ self changed: #font ].
	self whenStateChangedDo: [ self changed: #state ].
	self whenMenuChangedDo: [ self withAdapterDo: #updateMenu ].
	self property: #askBeforeChanging whenChangedDo: [ :newValue | self changed: {#askBeforeChanging: . newValue} ].
	self
		property: #shortcut
		whenChangedDo: [ :newShortcut :oldShortcut | 
			self unregisterShortcut: oldShortcut.
			self registerShortcut: newShortcut.
			self changed: #label ].

	self color: nil
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

	^ self shortcut ifNotNil: [ :s | s platformCharacter ]
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

{ #category : #'api-events' }
SpButtonPresenter >> whenActivatedDo: aBlock [ 
	
	^ self whenActionPerformedDo: aBlock 
]

{ #category : #'api-events' }
SpButtonPresenter >> whenFontChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	self property: #font whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpButtonPresenter >> whenStateChangedDo: aBlock [
	"set a block to perform after that the button has been aclicked, and its action performed"

	self property: #state whenChangedDo: aBlock
]
