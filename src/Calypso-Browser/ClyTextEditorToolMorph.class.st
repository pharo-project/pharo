"
I am a root for browser tools which requires text editor.
I install hooks into standard text morph to intercept all navigation requests and evaluated by Calypso logic.
Also I provide support for smart suggestions.

My subclasses should implement two methods: 

- editingText 
The text which is actually edited 

- applyChanges 
The operation to accept editing text changes 

Internal Representation and Key Implementation Points.

    Instance Variables
	applyingChanges:		<Boolean>
	changesCancelRequested:		<Boolean>
	textModel:		<RubScrolledTextModel>
	textMorph:		<RubScrolledTextMorph>
"
Class {
	#name : #ClyTextEditorToolMorph,
	#superclass : #ClyBrowserToolMorph,
	#instVars : [
		'textModel',
		'textMorph',
		'changesCancelRequested',
		'applyingChanges'
	],
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #accessing }
ClyTextEditorToolMorph >> activationPriorityInNonActiveContext [ 

	^applyingChanges
		ifTrue: [ Float infinity ]
		ifFalse: [ super activationPriorityInNonActiveContext ] 
]

{ #category : #operations }
ClyTextEditorToolMorph >> applyChanges [
	self subclassResponsibility 
]

{ #category : #'events handling' }
ClyTextEditorToolMorph >> applyChangesBy: aBlock [

	applyingChanges := true.
	^aBlock ensure: [ applyingChanges := false]
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> bindingOf: aString [ 
	^self selectedClassOrMetaClass bindingOf: aString
]

{ #category : #updating }
ClyTextEditorToolMorph >> browserContextWasChanged [
	"We should ignore any notifications during applying changes 
	because applied changes can cause them which can lead to wrong recursive updates"
	applyingChanges ifFalse: [ super browserContextWasChanged ]
]

{ #category : #building }
ClyTextEditorToolMorph >> build [	
	self buildTextMorph.
	textModel clearUndoManager.
	self subscribeOnTextChanges.
	
	textMorph 
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self addMorph: textMorph
]

{ #category : #building }
ClyTextEditorToolMorph >> buildLeftSideBar [
	textMorph withTextSegmentIcons
]

{ #category : #building }
ClyTextEditorToolMorph >> buildTextMorph [
	textModel := RubScrolledTextModel new.
	textModel interactionModel: self.
	textMorph := textModel newScrolledText.
	textMorph
		width: self width; "build is performed in background when owner is not exist yet. But we need proper width to perform kind of styling/formatting if needed"
		beWrapped;
		font: StandardFonts codeFont;
		editingMode: self editingMode.
	CmdKMDispatcher attachedTo: textMorph textArea withCommandsFrom: self. "It overrides default text morph shortcuts with Commander"
	self buildLeftSideBar.
	textModel setInitialText: self editingText.
	
]

{ #category : #operations }
ClyTextEditorToolMorph >> cancelChanges [

	changesCancelRequested := true.
	textMorph setText: self editingText.
	changesCancelRequested := false.
	self rebuildStatusBar
]

{ #category : #'events handling' }
ClyTextEditorToolMorph >> changesAccepted [

	| applied |
	^self applyChangesBy: [ 
		textMorph hasUnacceptedEdits: true.
		[self changesAreAboutApply.
		applied := self applyChanges] on: Error do: [ :err | 
			textModel text: self editingText. 
			textMorph hasUnacceptedEdits: true.
			err pass].	
		applied 
			ifTrue: [ textMorph hasUnacceptedEdits: false. 
				self textUpdated.
				browser focusActiveTab ]
			ifFalse: [ 
				textModel text: self editingText. 
				textMorph hasUnacceptedEdits: true].
	]
]

{ #category : #'events handling' }
ClyTextEditorToolMorph >> changesAreAboutApply [
	"it is a hook method to allow subclasses prepare state 
	just before applying changes"
]

{ #category : #'events handling' }
ClyTextEditorToolMorph >> changesCancelRequested: aRubCancelEditRequested [ 

	changesCancelRequested := true
]

{ #category : #'events handling' }
ClyTextEditorToolMorph >> changesCancelled [

	textMorph hasUnacceptedEdits: false.
	self textUpdated
]

{ #category : #contexts }
ClyTextEditorToolMorph >> createCommandContext [
	^self createTextContext
]

{ #category : #contexts }
ClyTextEditorToolMorph >> createTextContext [
	^ClyTextEditorContext for: self
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> doItContext [
	^nil
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> doItReceiver [
	^self selectedClassOrMetaClass ifNotNil: [:class | class instanceSide]
]

{ #category : #building }
ClyTextEditorToolMorph >> editingMode [
	| editingMode |
	editingMode := ClyTextEditingMode browserTool: self.
	editingMode
		classOrMetaClass: self selectedClassOrMetaClass;
		isForScripting: self isForScripting.
	^editingMode
]

{ #category : #accessing }
ClyTextEditorToolMorph >> editingText [
	self subclassResponsibility 
]

{ #category : #'selecting text' }
ClyTextEditorToolMorph >> findAnyString: strings in: text [

	| index |
	strings do: [ :each | 
		index := text findString: each startingAt: 1.
		index > 0 ifTrue: [^index to: index + each size]].
	
	^0 to: -1
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> guessTypeForName: aString [
	^nil
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> hasBindingOf: aString [ 
	^self selectedClassOrMetaClass hasBindingOf: aString
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> hasBindingThatBeginsWith: aString [ 
	^self selectedClassOrMetaClass 
		ifNil: [ false ]
		ifNotNil: [:c | c hasBindingThatBeginsWith: aString]
]

{ #category : #testing }
ClyTextEditorToolMorph >> hasUnacceptedEdits [
	textMorph ifNil: [ ^false ].
	
	^textMorph hasUnacceptedEdits
]

{ #category : #initialization }
ClyTextEditorToolMorph >> initialize [
	super initialize.
	changesCancelRequested := false.
	applyingChanges := false
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> isForScripting [
	^true
]

{ #category : #keymapping }
ClyTextEditorToolMorph >> kmDispatcher [

	^ CmdKMDispatcher attachedTo: self
]

{ #category : #accessing }
ClyTextEditorToolMorph >> leftSideBar [
	^textMorph rulerNamed: #textSegmentIcons
]

{ #category : #accessing }
ClyTextEditorToolMorph >> pendingText [
	^textMorph text
]

{ #category : #accessing }
ClyTextEditorToolMorph >> pendingText: aText [
	textMorph updateTextWith: aText.
	textMorph hasUnacceptedEdits: true
]

{ #category : #initialization }
ClyTextEditorToolMorph >> resetStateForSnapshot [
	super resetStateForSnapshot.
	
	textMorph := nil.
	textModel := nil
]

{ #category : #'selecting text' }
ClyTextEditorToolMorph >> selectAnyString: strings [

	textMorph setSelection: (self findAnyString: strings in: self pendingText)
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> selectedClassOrMetaClass [
	^nil
]

{ #category : #'smart suggestions support' }
ClyTextEditorToolMorph >> selectedMessage [
	^nil
]

{ #category : #accessing }
ClyTextEditorToolMorph >> selectedTextInterval [
	^textMorph selectionInterval
]

{ #category : #'rubric interaction model' }
ClyTextEditorToolMorph >> shoutAboutToStyle: aMorph [
	^self wantsTextStyling
]

{ #category : #building }
ClyTextEditorToolMorph >> subscribeOnTextChanges [

	textMorph announcer 		
		when: RubTextAccepted send: #changesAccepted to: self;
		when: RubTextChanged send: #textChanged: to: self;
		when: RubCancelEditRequested send: #changesCancelRequested: to: self
]

{ #category : #'focus management' }
ClyTextEditorToolMorph >> takeKeyboardFocus [ 
	textMorph ifNotNil: [ textMorph takeKeyboardFocus]
]

{ #category : #'events handling' }
ClyTextEditorToolMorph >> textChanged: aTextChanged [

	changesCancelRequested ifTrue: [ 
		"user cancel changes and now text morph shows original text model".
		changesCancelRequested := false.
		^self changesCancelled ].
	
	self updateDirtyState
]

{ #category : #accessing }
ClyTextEditorToolMorph >> textModel [
	^textModel
]

{ #category : #accessing }
ClyTextEditorToolMorph >> textMorph [
	^textMorph
]

{ #category : #updating }
ClyTextEditorToolMorph >> textUpdated [
	self applyDecorations.
	self updateDirtyState.
]

{ #category : #updating }
ClyTextEditorToolMorph >> triggerUpdate [
	applyingChanges ifFalse: [self update]
]

{ #category : #updating }
ClyTextEditorToolMorph >> update [
	| unacceptedText newText |
	super update.
	newText := self editingText.
	self pendingText asString = newText ifTrue: [ 
		self applyDecorations.
		^self ].

	self hasUnacceptedEdits ifTrue: [unacceptedText := self pendingText copy].
	textModel setInitialText: newText.
	unacceptedText ifNil: [ ^self textUpdated ].

	self pendingText: unacceptedText.
	textMorph hasEditingConflicts: true.
	textMorph changed
]

{ #category : #testing }
ClyTextEditorToolMorph >> wantsStayInDifferentContext [
	^super wantsStayInDifferentContext | applyingChanges
]

{ #category : #testing }
ClyTextEditorToolMorph >> wantsTextStyling [
	^true
]
