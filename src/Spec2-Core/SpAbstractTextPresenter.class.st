Class {
	#name : #SpAbstractTextPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#traits : 'SpTContextMenu',
	#classTraits : 'SpTContextMenu classTrait',
	#instVars : [
		'#hasUnacceptedEdits => SpObservableSlot',
		'#selection => SpObservableSlot',
		'#wantsVisualFeedback => SpObservableSlot',
		'#askBeforeDiscardingEdits => SpObservableSlot',
		'#text => SpObservableSlot',
		'#actionToPerform => SpObservableSlot',
		'#readSelection => SpObservableSlot',
		'#placeholder => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #testing }
SpAbstractTextPresenter class >> isAbstract [
	^ self = SpAbstractTextPresenter
]

{ #category : #private }
SpAbstractTextPresenter >> acceptText: aString [
	"Used to set the text value comming from the adapter (we do not want to re-send 
	 this value to the adapter, hence we set it 'raw', not triggering events)"

	self property: #text rawValue: aString
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> askBeforeDiscardingEdits [
	^ askBeforeDiscardingEdits
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> askBeforeDiscardingEdits: aBoolean [
	askBeforeDiscardingEdits := aBoolean
]

{ #category : #api }
SpAbstractTextPresenter >> clearContent [
	self text: ''
]

{ #category : #api }
SpAbstractTextPresenter >> clearSelection [
	"Remove selection from the text model"

	self selectionInterval: (0 to: 0)
]

{ #category : #'undo-redo' }
SpAbstractTextPresenter >> clearUndoManager [

	self withAdapterDo: [ :anAdapter | anAdapter clearUndoManager ]
]

{ #category : #api }
SpAbstractTextPresenter >> cursorPositionIndex [
	
	^self withAdapterDo: [ :anAdapter |
		anAdapter cursorPositionIndex ]
]

{ #category : #api }
SpAbstractTextPresenter >> enabled: aBoolean [

	super enabled: aBoolean.
	self 
		changed: #enabled: 
		with: { aBoolean }
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> eventKeyStrokesForNextFocus [
	"String describing the keystroke to perform to jump to the next widget"
	^ { Character arrowRight command ctrl asKeyCombination }
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> eventKeyStrokesForPreviousFocus [
	"String describing the keystroke to perform to jump to the previous widget"

	^ { Character arrowLeft command ctrl asKeyCombination }
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> hasEditingConflicts [
	"Return if the text zone has editing conflicts"

	^ self widget 
		ifNil: [ false ]
		ifNotNil: [:w | w hasEditingConflicts ]
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> hasEditingConflicts: aBoolean [
	"Set if the text zone has editing conflicts"
	
	^ self changed: #hasEditingConflicts: with: { aBoolean }
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> hasUnacceptedEdits [
	"Return if the text zone has unaccepted edits (orange corner)"

	^ hasUnacceptedEdits
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> hasUnacceptedEdits: aBoolean [
	"Return if the text zone has unaccepted edits (orange corner)"

	hasUnacceptedEdits := aBoolean
]

{ #category : #initialization }
SpAbstractTextPresenter >> initialize [
	super initialize.

	text := ''.
	placeholder := ''.
	actionToPerform := [ :txt |  ].
	readSelection := [ selection ].
	hasUnacceptedEdits := false.
	selection := 1 to: 0.
	wantsVisualFeedback := true.
	askBeforeDiscardingEdits := true.

	self registerEvents
]

{ #category : #api }
SpAbstractTextPresenter >> insert: aString at: positionIndex [
	"inserts aString into the text string. 
	 positionIndex indicates the position index (as in #indexOf) the insertion."
	
	self withAdapterDo: [ :anAdapter |
		anAdapter insert: aString at: positionIndex ]
]

{ #category : #api }
SpAbstractTextPresenter >> insertAndSelectAfterCurrentSelection: aString [
	| selectionInterval |

	self flag: #TODO. "Does this really has to be part of the text api?"
	selectionInterval := self selectionInterval.
	self insert: aString at: selectionInterval last.
	self selectionInterval: (selectionInterval last + 1 to: selectionInterval last + aString size)
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> isForSmalltalkCode [

	"self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8"

	^ false
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> isForSmalltalkCode: aBoolean [
	
	"self 
		deprecated: 'The forCode functionality has moved to TextCodePresenter, please use it instead TextPresenter.' 
		on: '2019-04-05' 
		in: #Pharo8"
]

{ #category : #localization }
SpAbstractTextPresenter >> localeChanged [

	super localeChanged.
	self forceUpdateSlot: #placeholder
	
]

{ #category : #'api-errors' }
SpAbstractTextPresenter >> notify: errorMessage at: position in: sourceCode [

	self changed: #notify:at:in: with: { errorMessage . position . sourceCode }
]

{ #category : #api }
SpAbstractTextPresenter >> placeholder [

	^ placeholder
]

{ #category : #accessing }
SpAbstractTextPresenter >> placeholder: aText [

	placeholder := aText
]

{ #category : #private }
SpAbstractTextPresenter >> rawSelection: anInterval [

	self property: #selection rawValue: anInterval
]

{ #category : #api }
SpAbstractTextPresenter >> readSelection [
	^ self readSelectionBlock cull: self text cull: self
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> readSelectionBlock [
	"Return the block used to calculate the text selection"

	^ readSelection
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> readSelectionBlock: aBlock [
	"Set the block used to calculate the text selection"

	^ readSelection := aBlock
]

{ #category : #initialization }
SpAbstractTextPresenter >> registerEvents [

	self whenTextChangedDo: [ self changed: #getText ].
	self whenReadSelectionIsChangedDo: [ self changed: #readSelection ].
	self property: #hasUnacceptedEdits whenChangedDo: [ :bool | self changed: #setHasUnacceptedEdits: with: {bool} ].
	self property: #selection whenChangedDo: [ :block | self changed: #setSelectionFromModel: with: {block value} ].
	self property: #wantsVisualFeedback whenChangedDo: [ :bool | self changed: #changed with: #() ].
	self property: #askBeforeDiscardingEdits whenChangedDo: [ :bool | self changed: #askBeforeDiscardingEdits: with: {bool} ]
]

{ #category : #api }
SpAbstractTextPresenter >> selectAll [
	
	self changed: #selectAll with: #()
]

{ #category : #api }
SpAbstractTextPresenter >> selectLine [
	self withAdapterDo: [ :anAdapter | anAdapter selectLine ].
	^ self selectionInterval
]

{ #category : #TOREMOVE }
SpAbstractTextPresenter >> selectedClassOrMetaClass [

	^ self behavior
]

{ #category : #api }
SpAbstractTextPresenter >> selectedText [ 
	| selectionInterval |
	
	selectionInterval := self selectionInterval.
	selectionInterval ifEmpty: [ ^ '' ].
	
	^ self text asString
		copyFrom: selectionInterval first 
		to: selectionInterval last
]

{ #category : #api }
SpAbstractTextPresenter >> selectionInterval [
	"Get the text selection interval.
	 I will update the selection from adapter first"
	
	self withAdapterDo: [ :anAdapter | 
		self rawSelection: anAdapter selection ].
	^ selection ifNil: [ ^ 1 to: 0 ]
]

{ #category : #api }
SpAbstractTextPresenter >> selectionInterval: anInterval [
	"Set the text selection interval without changing the readSelectionBlock"

	selection := anInterval
]

{ #category : #api }
SpAbstractTextPresenter >> text [

	^ text
]

{ #category : #api }
SpAbstractTextPresenter >> text: aText [
	"Set the text of the text zone"

	text := aText
]

{ #category : #simulating }
SpAbstractTextPresenter >> type: aString [ 
	"For simulation: Do as if we type the given string"
	self text: aString
]

{ #category : #api }
SpAbstractTextPresenter >> wantsVisualFeedback [
	^ wantsVisualFeedback
]

{ #category : #api }
SpAbstractTextPresenter >> wantsVisualFeedback: aBoolean [
	wantsVisualFeedback := aBoolean
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenAboutToStyleChangedDo: aBlock [
	"Set a block to perform when the about to style block changed"
	
	self whenAboutToStyleBlockChanged: [:block :old :announcement :anAnnouncer | 
		aBlock cull: (block value) cull: old cull: announcement cull: anAnnouncer ]
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenAcceptBlockChangedDo: aBlock [
	"Set a block to perform when the accept block changed"
	
	self property: #actionToPerform whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenPlaceholderChangedDo: aBlock [
	
	self property: #placeholder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenReadSelectionIsChangedDo: aBlock [
	"Set a block to perform when the read selection block changed"

	self property: #readSelection whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenResetDo: aBlock [
	"This method will add a 'reset' event to the widget. It will react when user presses meta+l 
	key (this is for historical reasons)."

	self 
		bindKeyCombination: $l meta 
		toAction: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenSelectionChangedDo: aBlock [
	"Set a block to perform when the selection changed"

	self property: #selection whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenSubmitDo: aBlock [
	"This method will add a 'submit' event to the widget. It will react when user presses meta+s 
	key (this is for historical reasons) and (in case of text fields) with enter."

	self subclassResponsibility
]

{ #category : #'api-events' }
SpAbstractTextPresenter >> whenTextChangedDo: aBlock [
	"Set a block to perform when the text changed"

	self property: #text whenChangedDo: aBlock
]
