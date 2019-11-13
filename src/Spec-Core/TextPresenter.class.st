"
I handle basic multi-line text.
See AbstractWidgetPresenter

self example

You can also have a look at ListSelectionPresenter and ScrollSyncExample for more examples.

My main purpose is to handle source code (I have editing shortcuts support).

I provide the following variables and their accessors
- autoAccept is a boolean representing if the text is accepted on each keystroke.
- text is the text of the text zone.

I provide the following methods
- beForCode and beForText are shortcut for configuring me.
- getSelection return the interval currently selected.
- getText is a getter of the text


todo
- aboutToStyleBlock  is the block used to know if the text must be styled, it returns a boolean. aboutToStyle: is a shortcut to set directly the boolean.
- accept , accept:notifying:
- acceptBlock should convert the text in a boolean representing if it is to be accepted.
- askBeforeDiscardingEdits
- behavior
- clearSelection
- codePaneMenu:shifted:
- doItContext
- doItReceiver
- specializing eventKeyStrokesForNextFocus , eventKeyStrokesForPreviousFocus
- getMenu
- hasEditingConflicts
- hasUnacceptedEdits
- isAboutToStyle
- isCodeCompletionAllowed
- isForSmalltalkCode
- menuHolder
- notify:at:in:
- readSelection , readSelectionBlock
- registerEvents , registerEventsForShout
- scrollValue
- selectedBehavior
- selectedClassOrMetaClass
-  setSelection: select the text in the interval in argument, selectAll is a shorcut that set the larger interval.
- setSelectionInterval:
- textArea

"
Class {
	#name : #TextPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'aboutToStyleHolder',
		'behaviorHolder',
		'textHolder',
		'actionToPerformHolder',
		'readSelectionHolder',
		'actionPerformedHolder',
		'isCodeCompletionAllowedHolder',
		'doItContext',
		'doItReceiver',
		'hasUnacceptedEdits',
		'selection',
		'scrollValue',
		'menuHolder',
		'autoAccept',
		'wantsVisualFeedback',
		'askBeforeDiscardingEdits',
		'isForSmalltalkCodeHolder'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
TextPresenter class >> adapterName [

	^ #TextAdapter
]

{ #category : #specs }
TextPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(TextAdapter
		adapt: #(model))
]

{ #category : #example }
TextPresenter class >> example [
	<sampleInstance>
	^ self new
	 openWithSpec;
		text:
			'This is a test.
If this had been an actual emergency...'

]

{ #category : #specs }
TextPresenter class >> title [

	^ 'Text'
]

{ #category : #'api-shout' }
TextPresenter >> aboutToStyle: aBoolean [	
	"<api: #boolean getter: #isAboutToStyle registration: #whenAboutToStyleChanged:>"
	"Set if the text zone must be styled"
	
	aboutToStyleHolder value: [ aBoolean ]
]

{ #category : #'api-shout' }
TextPresenter >> aboutToStyleBlock [
	"Return the block used to know if the text must be styled"	
	"<api:#inspect>"
	^ aboutToStyleHolder value
]

{ #category : #'api-shout' }
TextPresenter >> aboutToStyleBlock: aBlockReturningABoolean [
	"<api: #block getter: #aboutToStyleBlock registration: #whenAboutToStyleBlockChanged:>"
	"Set the block used to know if the text must be styled.
	The block must return a boolean"

	aboutToStyleHolder value: aBlockReturningABoolean
]

{ #category : #api }
TextPresenter >> accept [
	"<api: #do>"
	"Accep the current pendingtext"
	
	self changed: #accept with: #().
	
	"Dummy changed to raise an announcement"
	actionPerformedHolder value: self text.
]

{ #category : #api }
TextPresenter >> accept: aText notifying: aNotifyier [
	| accept newText |
	
	accept := actionToPerformHolder value cull: aText cull: aNotifyier.
	accept == false ifTrue: [ ^ false ].
	
	newText := aText copy.
	textHolder value: newText.
	
	"Dummy changed to raise an announcement"
	actionPerformedHolder value: newText.
	
	self changed: #clearUserEdits.
	^ true
]

{ #category : #api }
TextPresenter >> acceptBlock [
	"<api: #inspect>"
	"Return the block to perform when the text is accepted.
	The block must have one argument, which will be the accepted text"

	^ actionToPerformHolder value
]

{ #category : #api }
TextPresenter >> acceptBlock: aBlockWithOneArgument [
	"<api: #block getter: #acceptBlock registration: #whenAcceptBlockChanged:>"
	"Set the block to perform when the text is accepted.
	The block must have one argument, which will be the accepted text.
	The block can return false if the candidate text is to be rejected"

	actionToPerformHolder value: aBlockWithOneArgument
]

{ #category : #api }
TextPresenter >> askBeforeDiscardingEdits [

	^ askBeforeDiscardingEdits value
]

{ #category : #api }
TextPresenter >> askBeforeDiscardingEdits: aBoolean [

	askBeforeDiscardingEdits value: aBoolean
]

{ #category : #api }
TextPresenter >> autoAccept [ 
	"Return true if the text is accepted after ech keystroke"
	"<api:#inspect>"
	^ autoAccept value
]

{ #category : #api }
TextPresenter >> autoAccept: aBoolean [
	"<api: #boolean getter: #autoAccept registration: #whenAutoAcceptChanged:>"
	"Set if the text is accepted after each keystroke or not"

	autoAccept value: aBoolean
]

{ #category : #api }
TextPresenter >> beForCode [
	self
		isCodeCompletionAllowed: true;
		menuHolder: [ self getMenu ];
		isForSmalltalkCode: true
]

{ #category : #api }
TextPresenter >> beForText [
	self
		isCodeCompletionAllowed: false;
		menuHolder: [ self getMenu ];
		isForSmalltalkCode: false
]

{ #category : #'api-shout' }
TextPresenter >> behavior [
	"<api: #inspect>"
	"Return the class corresponding to the method class of the source code you are editing"
	
	^ behaviorHolder value
]

{ #category : #'api-shout' }
TextPresenter >> behavior: aClass [
	"< api: #st getter: #behavior registration: #whenBehaviorChanged:>"
	"Set the class corresponding to the method class of the source code you are editing"

	behaviorHolder value: aClass
]

{ #category : #api }
TextPresenter >> clearSelection [
"	<api: #do>"
   "Remove selection from the text model"

	self setSelection: (0 to: 0)
]

{ #category : #'undo-redo' }
TextPresenter >> clearUndoManager [
	self widget ifNotNil: #clearUndoManager
]

{ #category : #api }
TextPresenter >> codePaneMenu: aMenu shifted: shifted [ 
	"Note that unless we override perform:orSendTo:, 
	PluggableTextController will respond to all menu items in a 
	text pane"
	
	^ menuHolder value cull: aMenu cull: shifted
]

{ #category : #'api-doIt' }
TextPresenter >> doItContext [

	^ doItContext value
]

{ #category : #'api-doIt' }
TextPresenter >> doItContext: anObject [

	doItContext value: anObject
]

{ #category : #'api-doIt' }
TextPresenter >> doItReceiver [
	| receiver |
	
	receiver := doItReceiver value.
	receiver isBehavior ifTrue: [receiver  := receiver instanceSide].
	^receiver
]

{ #category : #'api-doIt' }
TextPresenter >> doItReceiver: anObject [

	doItReceiver value: anObject
]

{ #category : #focus }
TextPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"
	^ { Character arrowRight command ctrl asKeyCombination }
]

{ #category : #focus }
TextPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"

	^ { Character arrowLeft command ctrl asKeyCombination }
]

{ #category : #private }
TextPresenter >> getMenu [
	^ (PragmaMenuBuilder
		pragmaKeyword: self textArea editingModeClass menuKeyword
		model: self widget) menu
]

{ #category : #api }
TextPresenter >> getSelection [
	"<api:#inspect>"
	"Get the text selection"

	selection ifNil: [ ^(1 to: 0) ].
	^ selection value
]

{ #category : #api }
TextPresenter >> getText [
	"<api: #inspect>"
	^ textHolder value
]

{ #category : #api }
TextPresenter >> hasEditingConflicts [
	"<api:#inspect>"
	"Return if the text zone has editing conflicts"

	^ self widget 
		ifNil: [ false ]
		ifNotNil: [:w | w hasEditingConflicts ]
]

{ #category : #api }
TextPresenter >> hasEditingConflicts: aBoolean [
	"Set if the text zone has editing conflicts"
	"<api:#boolean getter: #hasEditingConflicts>"
	
	^ self changed: #hasEditingConflicts: with: { aBoolean }
]

{ #category : #api }
TextPresenter >> hasUnacceptedEdits [
	"<api:#inspect>"
	"Return if the text zone has unaccepted edits (orange corner)"

	hasUnacceptedEdits ifNil: [ ^ false ].
	^ hasUnacceptedEdits value
]

{ #category : #api }
TextPresenter >> hasUnacceptedEdits: aBoolean [
	"Return if the text zone has unaccepted edits (orange corner)"
	"<api:#boolean getter: #hasUnacceptedEdits>"
	hasUnacceptedEdits value: aBoolean
]

{ #category : #initialization }
TextPresenter >> initialize [
	super initialize.
	textHolder := '' asValueHolder.
	behaviorHolder := nil asValueHolder.
	aboutToStyleHolder := [ false ] asValueHolder.
	actionToPerformHolder := [ :text |  ] asValueHolder.
	readSelectionHolder := [ selection value ] asValueHolder.
	actionPerformedHolder := '' asValueHolder.
	isCodeCompletionAllowedHolder := false asValueHolder.
	isForSmalltalkCodeHolder := false asValueHolder.
	doItContext := nil asValueHolder.
	doItReceiver := nil asValueHolder.
	hasUnacceptedEdits := false asValueHolder.
	selection := (1 to: 0) asValueHolder.
	scrollValue := (0 @ 0) asValueHolder.
	menuHolder := [ :menu :shifted |  ] asValueHolder.
	autoAccept := false asValueHolder.
	wantsVisualFeedback := true asValueHolder.
	askBeforeDiscardingEdits := true asValueHolder.
	self beForText.
	self registerEvents.
	self registerEventsForShout
]

{ #category : #'api-shout' }
TextPresenter >> isAboutToStyle [
	"<api: #inspect>"
	"Return if the text zone is shouted or not"

	^ self aboutToStyleBlock value
]

{ #category : #api }
TextPresenter >> isCodeCompletionAllowed: aBoolean [
	"<api: #boolean getter: #isCodeCompletionAllowed registration: #whenCodeCompletionAllowedChanged:>"
	"Set if code completion is allowed"
	
	isCodeCompletionAllowedHolder value: aBoolean
]

{ #category : #api }
TextPresenter >> isForSmalltalkCode [
	^ isForSmalltalkCodeHolder value
]

{ #category : #api }
TextPresenter >> isForSmalltalkCode: aBoolean [
	isForSmalltalkCodeHolder value: aBoolean
]

{ #category : #api }
TextPresenter >> menuHolder [
	"<api: #inspect>"
	"Return the block used to calculate the text selection"

	^ menuHolder value
]

{ #category : #api }
TextPresenter >> menuHolder: aBlock [
	"<api: #block getter: #menuHolder registration: #whenMenuHolderChanged:>"
	"Set the block used to compute the menu"

	^ menuHolder value:  aBlock
]

{ #category : #'api-errors' }
TextPresenter >> notify: errorMessage at: position in: sourceCode [

	self changed: #notify:at:in: with: { errorMessage . position . sourceCode }
]

{ #category : #api }
TextPresenter >> readSelection [
	"<api: #inspect>"
	^ readSelectionHolder value cull: self text cull: self
]

{ #category : #api }
TextPresenter >> readSelectionBlock [
	"<api: #inspect>"
	"Return the block used to calculate the text selection"

	^ readSelectionHolder value
]

{ #category : #api }
TextPresenter >> readSelectionBlock: aBlock [
	"<api: #block getter: #readSelectionBlock registration: #whenReadSelectionIsChanged:>"
	"Set the block used to calculate the text selection"

	^ readSelectionHolder value: aBlock
]

{ #category : #initialization }
TextPresenter >> registerEvents [

	textHolder whenChangedDo: [ self changed: #getText ].
	readSelectionHolder whenChangedDo: [ self changed: #readSelection ].
	hasUnacceptedEdits whenChangedDo: [ :bool | self changed: #setHasUnacceptedEdits: with: { bool } ].
	selection whenChangedDo: [:block | self changed: #setSelectionFromModel: with: { block value } ].
	scrollValue whenChangedDo: [ :newPosition | self changed: #setScrollValue: with: { newPosition } ].
	autoAccept whenChangedDo: [:bool | self changed: #autoAccept: with: { bool } ].
	wantsVisualFeedback whenChangedDo: [ :bool | self changed: #changed with: #() ].
	askBeforeDiscardingEdits whenChangedDo: [ :bool | self changed: #askBeforeDiscardingEdits: with: { bool } ].
]

{ #category : #initialization }
TextPresenter >> registerEventsForShout [

	aboutToStyleHolder whenChangedDo: [ self changed: #getText ].
	behaviorHolder
		whenChangedDo: [ :class | 
			self changed: #classOrMetaClass: with: { class }.
			self changed: #getText ]
]

{ #category : #api }
TextPresenter >> scrollValue [
	"<api: #inspect>"
	"Return the current scroll position"

	^ scrollValue value
]

{ #category : #api }
TextPresenter >> scrollValue: aPoint [
	"<api: #point getter: #scrollValue registration: #whenScrollValuehanged:>"
	"Set the scroll position"

	^ scrollValue value: aPoint
]

{ #category : #api }
TextPresenter >> selectAll [
	
	self changed: #selectAll with: #()
]

{ #category : #api }
TextPresenter >> selectedBehavior [

	^ behaviorHolder value
]

{ #category : #NOCompletion }
TextPresenter >> selectedClassOrMetaClass [

	^ self behavior
]

{ #category : #api }
TextPresenter >> setSelection: anInterval [
	"Set the text selection without changing the readSelectionBlock"
	"<api: #st getter: #getSelection>"

	selection ifNotNil: [: holder | holder value: anInterval ]
]

{ #category : #api }
TextPresenter >> setSelectionInterval: anInterval [
	selection rawValue: anInterval
]

{ #category : #api }
TextPresenter >> text [

	^ self getText
]

{ #category : #api }
TextPresenter >> text: aText [
	"<api: #string getter: #getText registration: #whenTextChanged:>"
	"Set the text of the text zone"
	
	textHolder value: aText
]

{ #category : #private }
TextPresenter >> textArea [
	^ self widget widget 
]

{ #category : #api }
TextPresenter >> wantsVisualFeedback [

	^ wantsVisualFeedback value
]

{ #category : #api }
TextPresenter >> wantsVisualFeedback: aBoolean [

	wantsVisualFeedback value: aBoolean
]

{ #category : #'api-events' }
TextPresenter >> whenAboutToStyleBlockChanged: aBlock [
	"Set a block to perform when the about to style block changed"
	
	aboutToStyleHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenAboutToStyleChanged: aBlock [
	"<api: #event>"
	"Set a block to perform when the about to style block changed"
	
	self whenAboutToStyleBlockChanged: [:block :old :announcement :anAnnouncer | 
		aBlock cull: (block value) cull: old cull: announcement cull: anAnnouncer ]
]

{ #category : #'api-events' }
TextPresenter >> whenAcceptBlockChanged: aBlock [
	"Set a block to perform when the accept block changed"
	
	actionToPerformHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenAutoAcceptChanged: aBlock [
	"aBlock will be performed when autoAccept changed"
	
	autoAccept whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenBehaviorChanged: aBlock [
	"Set a block to perform when the behavior class changed"
	
	behaviorHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenCodeCompletionAllowedChanged: aBlock [
	"Set a block to perform when the code completion value changed"
	
	actionPerformedHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenMenuHolderChanged: aBlock [
	"Set a block to perform when the block use to build menu changed"
	
	menuHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenReadSelectionIsChanged: aBlock [
	"Set a block to perform when the read selection block changed"
	
	readSelectionHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenScrollValueChanged: aBlock [
	"Set a block to perform when the scroll value changed"
	
	scrollValue whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenTextChanged: aBlock [
	"Set a block to perform when the text changed"
	
	textHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TextPresenter >> whenTextIsAccepted: aBlock [
	"Set a block to perform when the text is accepted"
	
	actionPerformedHolder whenChangedDo: aBlock
]
