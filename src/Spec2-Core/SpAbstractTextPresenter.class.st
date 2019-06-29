Class {
	#name : #SpAbstractTextPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#textHolder',
		'#actionToPerformHolder',
		'#readSelectionHolder',
		'#actionPerformedHolder',
		'#hasUnacceptedEdits',
		'#selection',
		'#autoAccept',
		'#wantsVisualFeedback',
		'#askBeforeDiscardingEdits',
		'#contextMenu => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #'api-shout' }
SpAbstractTextPresenter >> aboutToStyle: aBoolean [	
	"<api: #boolean getter: #isAboutToStyle registration: #whenAboutToStyleChanged:>"
	"Set if the text zone must be styled"
	
	self 
		deprecated: 'This has been moved to specific presenters' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #'api-shout' }
SpAbstractTextPresenter >> aboutToStyleBlock [
	"Return the block used to know if the text must be styled"	
	"<api:#inspect>"

	self 
		deprecated: 'This has been moved to specific presenters' 
		on: '2019-04-15'
		in: #Pharo8

]

{ #category : #'api-shout' }
SpAbstractTextPresenter >> aboutToStyleBlock: aBlockReturningABoolean [
	"<api: #block getter: #aboutToStyleBlock registration: #whenAboutToStyleBlockChanged:>"
	"Set the block used to know if the text must be styled.
	The block must return a boolean"

	self 
		deprecated: 'This has been moved to specific presenters' 
		on: '2019-04-15'
		in: #Pharo8

]

{ #category : #api }
SpAbstractTextPresenter >> accept [
	"<api: #do>"
	"Accep the current pendingtext"
	
	self changed: #accept with: #().
	
	"Dummy changed to raise an announcement"
	actionPerformedHolder value: self text.
]

{ #category : #api }
SpAbstractTextPresenter >> accept: aText notifying: aNotifyier [
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
SpAbstractTextPresenter >> acceptBlock [
	"<api: #inspect>"
	"Return the block to perform when the text is accepted.
	The block must have one argument, which will be the accepted text"

	^ actionToPerformHolder value
]

{ #category : #api }
SpAbstractTextPresenter >> acceptBlock: aBlockWithOneArgument [
	"<api: #block getter: #acceptBlock registration: #whenAcceptBlockChanged:>"
	"Set the block to perform when the text is accepted.
	The block must have one argument, which will be the accepted text.
	The block can return false if the candidate text is to be rejected"

	actionToPerformHolder value: aBlockWithOneArgument
]

{ #category : #private }
SpAbstractTextPresenter >> acceptText: aString [
	"Used to set the text value comming from the adapter (we do not want to re-send 
	 this value to the adapter, hence we set it 'raw', not triggering events)"

	textHolder rawValue: aString
]

{ #category : #api }
SpAbstractTextPresenter >> askBeforeDiscardingEdits [

	^ askBeforeDiscardingEdits value
]

{ #category : #api }
SpAbstractTextPresenter >> askBeforeDiscardingEdits: aBoolean [

	askBeforeDiscardingEdits value: aBoolean
]

{ #category : #api }
SpAbstractTextPresenter >> autoAccept [ 
	"Return true if the text is accepted after ech keystroke"
	"<api:#inspect>"
	^ autoAccept value
]

{ #category : #api }
SpAbstractTextPresenter >> autoAccept: aBoolean [
	"<api: #boolean getter: #autoAccept registration: #whenAutoAcceptChanged:>"
	"Set if the text is accepted after each keystroke or not"

	autoAccept value: aBoolean
]

{ #category : #api }
SpAbstractTextPresenter >> beForCode [

	self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8
]

{ #category : #api }
SpAbstractTextPresenter >> beForText [

	self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8
]

{ #category : #'api-shout' }
SpAbstractTextPresenter >> behavior [
	"Return the class corresponding to the method class of the source code you are editing"

	self 
		deprecated: 'This has been moved to specific presenters' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #'api-shout' }
SpAbstractTextPresenter >> behavior: aClass [
	"Set the class corresponding to the method class of the source code you are editing"

	self 
		deprecated: 'This has been moved to specific presenters' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #api }
SpAbstractTextPresenter >> clearSelection [
"	<api: #do>"
   "Remove selection from the text model"

	self setSelection: (0 to: 0)
]

{ #category : #'undo-redo' }
SpAbstractTextPresenter >> clearUndoManager [

	self withAdapterDo: #clearUndoManager
]

{ #category : #api }
SpAbstractTextPresenter >> contextMenu [

	^ contextMenu
]

{ #category : #api }
SpAbstractTextPresenter >> contextMenu: aValuable [

	contextMenu := aValuable
]

{ #category : #'api-doIt' }
SpAbstractTextPresenter >> doItContext [

	self 
		deprecated: 'This has been moved to specific presenters (check children)' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #'api-doIt' }
SpAbstractTextPresenter >> doItContext: anObject [

	self 
		deprecated: 'This has been moved to specific presenters (check children)' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #'api-doIt' }
SpAbstractTextPresenter >> doItReceiver [

	self 
		deprecated: 'This has been moved to specific presenters (check children)' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #'api-doIt' }
SpAbstractTextPresenter >> doItReceiver: anObject [

	self 
		deprecated: 'This has been moved to specific presenters (check children)' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #api }
SpAbstractTextPresenter >> enabled: aBoolean [

	super enabled: aBoolean.
	self 
		changed: #enabled: 
		with: { aBoolean }
]

{ #category : #focus }
SpAbstractTextPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"
	^ { Character arrowRight command ctrl asKeyCombination }
]

{ #category : #focus }
SpAbstractTextPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"

	^ { Character arrowLeft command ctrl asKeyCombination }
]

{ #category : #private }
SpAbstractTextPresenter >> getMenu [
	self flag: #TODO.	"move to menu presenters"
	^ (PragmaMenuBuilder
		pragmaKeyword: self textArea editingModeClass menuKeyword
		model: self adapter) menu
]

{ #category : #api }
SpAbstractTextPresenter >> getSelection [
	"<api:#inspect>"
	"Get the text selection"

	selection ifNil: [ ^(1 to: 0) ].
	^ selection value
]

{ #category : #api }
SpAbstractTextPresenter >> getText [
	"<api: #inspect>"
	^ textHolder value
]

{ #category : #api }
SpAbstractTextPresenter >> hasEditingConflicts [
	"<api:#inspect>"
	"Return if the text zone has editing conflicts"

	^ self widget 
		ifNil: [ false ]
		ifNotNil: [:w | w hasEditingConflicts ]
]

{ #category : #api }
SpAbstractTextPresenter >> hasEditingConflicts: aBoolean [
	"Set if the text zone has editing conflicts"
	"<api:#boolean getter: #hasEditingConflicts>"
	
	^ self changed: #hasEditingConflicts: with: { aBoolean }
]

{ #category : #api }
SpAbstractTextPresenter >> hasUnacceptedEdits [
	"<api:#inspect>"
	"Return if the text zone has unaccepted edits (orange corner)"

	hasUnacceptedEdits ifNil: [ ^ false ].
	^ hasUnacceptedEdits value
]

{ #category : #api }
SpAbstractTextPresenter >> hasUnacceptedEdits: aBoolean [
	"Return if the text zone has unaccepted edits (orange corner)"
	"<api:#boolean getter: #hasUnacceptedEdits>"
	hasUnacceptedEdits value: aBoolean
]

{ #category : #initialization }
SpAbstractTextPresenter >> initialize [

	super initialize.

	textHolder := '' asValueHolder.
	actionToPerformHolder := [ :text |  ] asValueHolder.
	readSelectionHolder := [ selection value ] asValueHolder.
	actionPerformedHolder := '' asValueHolder.
	hasUnacceptedEdits := false asValueHolder.
	selection := (1 to: 0) asValueHolder.
	autoAccept := false asValueHolder.
	wantsVisualFeedback := true asValueHolder.
	askBeforeDiscardingEdits := true asValueHolder.

	self registerEvents
]

{ #category : #NOCompletion }
SpAbstractTextPresenter >> isCodeCompletionAllowed [
	"Return if code completion is allowed"
	
	"self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8"
		
	^ false
]

{ #category : #api }
SpAbstractTextPresenter >> isCodeCompletionAllowed: aBoolean [
	"<api: #boolean getter: #isCodeCompletionAllowed registration: #whenCodeCompletionAllowedChanged:>"
	"Set if code completion is allowed"
	
	"self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8"
]

{ #category : #api }
SpAbstractTextPresenter >> isForSmalltalkCode [

	"self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8"

	^ false
]

{ #category : #api }
SpAbstractTextPresenter >> isForSmalltalkCode: aBoolean [
	
	"self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8"
]

{ #category : #'api-errors' }
SpAbstractTextPresenter >> notify: errorMessage at: position in: sourceCode [

	self changed: #notify:at:in: with: { errorMessage . position . sourceCode }
]

{ #category : #api }
SpAbstractTextPresenter >> readSelection [
	"<api: #inspect>"
	^ readSelectionHolder value cull: self text cull: self
]

{ #category : #api }
SpAbstractTextPresenter >> readSelectionBlock [
	"<api: #inspect>"
	"Return the block used to calculate the text selection"

	^ readSelectionHolder value
]

{ #category : #api }
SpAbstractTextPresenter >> readSelectionBlock: aBlock [
	"<api: #block getter: #readSelectionBlock registration: #whenReadSelectionIsChanged:>"
	"Set the block used to calculate the text selection"

	^ readSelectionHolder value: aBlock
]

{ #category : #initialization }
SpAbstractTextPresenter >> registerEvents [

	textHolder whenChangedDo: [ self changed: #getText ].
	readSelectionHolder whenChangedDo: [ self changed: #readSelection ].
	hasUnacceptedEdits whenChangedDo: [ :bool | self changed: #setHasUnacceptedEdits: with: { bool } ].
	selection whenChangedDo: [:block | self changed: #setSelectionFromModel: with: { block value } ].
	autoAccept whenChangedDo: [:bool | self changed: #autoAccept: with: { bool } ].
	wantsVisualFeedback whenChangedDo: [ :bool | self changed: #changed with: #() ].
	askBeforeDiscardingEdits whenChangedDo: [ :bool | self changed: #askBeforeDiscardingEdits: with: { bool } ].
]

{ #category : #api }
SpAbstractTextPresenter >> selectAll [
	
	self changed: #selectAll with: #()
]

{ #category : #NOCompletion }
SpAbstractTextPresenter >> selectedClassOrMetaClass [

	^ self behavior
]

{ #category : #api }
SpAbstractTextPresenter >> setSelection: anInterval [
	"Set the text selection without changing the readSelectionBlock"
	"<api: #st getter: #getSelection>"

	selection ifNotNil: [: holder | holder value: anInterval ]
]

{ #category : #api }
SpAbstractTextPresenter >> setSelectionInterval: anInterval [
	selection rawValue: anInterval
]

{ #category : #api }
SpAbstractTextPresenter >> text [

	^ self getText
]

{ #category : #api }
SpAbstractTextPresenter >> text: aText [
	"<api: #string getter: #getText registration: #whenTextChanged:>"
	"Set the text of the text zone"
	
	textHolder value: aText
]

{ #category : #private }
SpAbstractTextPresenter >> textArea [
	^ self adapter widget
]

{ #category : #simulating }
SpAbstractTextPresenter >> type: aString [ 
	"For simulation: Do as if we type the given string"
	self text: aString
]

{ #category : #api }
SpAbstractTextPresenter >> wantsVisualFeedback [

	^ wantsVisualFeedback value
]

{ #category : #api }
SpAbstractTextPresenter >> wantsVisualFeedback: aBoolean [

	wantsVisualFeedback value: aBoolean
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenAboutToStyleBlockChangedDo: aBlock [
	"Set a block to perform when the about to style block changed"
	
	self 
		deprecated: 'This has been moved to specific presenters' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenAboutToStyleChangedDo: aBlock [
	"<api: #event>"
	"Set a block to perform when the about to style block changed"
	
	self whenAboutToStyleBlockChanged: [:block :old :announcement :anAnnouncer | 
		aBlock cull: (block value) cull: old cull: announcement cull: anAnnouncer ]
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenAcceptBlockChangedDo: aBlock [
	"Set a block to perform when the accept block changed"
	
	actionToPerformHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenAutoAcceptChangedDo: aBlock [
	"aBlock will be performed when autoAccept changed"
	
	autoAccept whenChangedDo: aBlock
]

{ #category : #enumerating }
SpAbstractTextPresenter >> whenBehaviorChangedDo: aBlock [
	"Set a block to perform when the behavior class changed"
	
	self 
		deprecated: 'This has been moved to specific presenters' 
		on: '2019-04-15'
		in: #Pharo8
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenCodeCompletionAllowedChangedDo: aBlock [
	"Set a block to perform when the code completion value changed"
	
	actionPerformedHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenMenuChangedDo: aBlock [
	"Set a block to value when the menu block has changed"
	
	self property: #contextMenu whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenReadSelectionIsChangedDo: aBlock [
	"Set a block to perform when the read selection block changed"
	
	readSelectionHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenTextChangedDo: aBlock [
	"Set a block to perform when the text changed"
	
	textHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenTextIsAcceptedDo: aBlock [
	"Set a block to perform when the text is accepted"
	
	actionPerformedHolder whenChangedDo: aBlock
]
