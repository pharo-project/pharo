"
I am the adapter used to bridge a TextPresenter and a PluggableTextMorph
"
Class {
	#name : #MorphicTextAdapter,
	#superclass : #AbstractMorphicAdapter,
	#traits : 'TViewModel',
	#classTraits : 'TViewModel classTrait',
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #'spec protocol' }
MorphicTextAdapter >> accept [
	"We use RubScrolledTextMorph>>#acceptContents but we keep accept as method name for compatibility"
	self widgetDo: [ :w | w acceptContents ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> accept: aText notifying: aNotifyier [

	self model accept: aText notifying: aNotifyier
]

{ #category : #accessing }
MorphicTextAdapter >> acceptAllowed [
	^self widget editingMode acceptAllowed
]

{ #category : #accessing }
MorphicTextAdapter >> announcer [
	^ self model announcer

	
]

{ #category : #'widget API' }
MorphicTextAdapter >> askBeforeDiscardingEdits [

	^ self model askBeforeDiscardingEdits
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> askBeforeDiscardingEdits: aBoolean [
	self widget askBeforeDiscardingEdits: aBoolean
]

{ #category : #'widget API' }
MorphicTextAdapter >> autoAccept [

	^ self model autoAccept
]

{ #category : #'widget API' }
MorphicTextAdapter >> autoAccept: aBoolean [

	^ self model autoAccept: aBoolean
]

{ #category : #factory }
MorphicTextAdapter >> buildWidget [
	| newWidget |
	self flag: #pharo7.	"When support for P7 will be dropped, remove this compatibility hack."
	newWidget := ((self class environment at: #SpecRubScrolledTextMorph ifAbsent: [ RubScrolledTextMorph ]) on: self)
		getTextSelector: #getText;
		setTextSelector: #accept:notifying:;
		getSelectionSelector: #readSelection;
		menuProvider: self selector: #codePaneMenu:shifted:;
		setSelectionSelector: #setSelection:;
		beWrapped;
		enabled: self enabled;
		askBeforeDiscardingEdits: self askBeforeDiscardingEdits;
		autoAccept: self autoAccept;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		registerScrollChanges: #scrollValueChanged:;
		yourself.
	self setEditingModeFor: newWidget.
	self model additionalKeyBindings ifNotNil: [ :bindings | bindings keysAndValuesDo: [ :shortcut :action | newWidget bindKeyCombination: shortcut toAction: action ] ].
	self presenter whenTextChangedDo: [ :text | newWidget setText: text ].
	^ newWidget
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> canChangeText [
	^ self widget textArea canChangeText
]

{ #category : #'undo-redo' }
MorphicTextAdapter >> clearUndoManager [
	self widget textArea editingState clearUndoManager: nil
]

{ #category : #'widget API' }
MorphicTextAdapter >> clearUserEditFlag [

	self model text: self getText.
	self changed: #clearUserEdits
]

{ #category : #'widget API' }
MorphicTextAdapter >> codePaneMenu: aMenu shifted: shifted [ 
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpecBindings
		value: self model application adapterBindings
		during: [ menuPresenter value buildWithSpec ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> font: aFont [
	self widgetDo: [ :w | w font: aFont ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> getText [

	^ self model text
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> hasEditingConflicts: aBoolean [
 
	self widgetDo: [ :w | w hasEditingConflicts: aBoolean ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> hasUnacceptedEdits: aBoolean [

	self model hasUnacceptedEdits: aBoolean
]

{ #category : #testing }
MorphicTextAdapter >> isCodeCompletionAllowed [

	^ false
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> notify: errorMessage at: position in: sourceCode [

	self flag: #TOMOVE.
	self widgetDo: [ :w | w notify: errorMessage at: position in: sourceCode ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> pendingText [

	^ self widgetDo: [ :w | w text ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> pendingText: aText [

	self widgetDo: [ :w | 
		w setText: aText.
		w hasUnacceptedEdits: true ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> readSelection [

	^ self model readSelection
]

{ #category : #'widget API' }
MorphicTextAdapter >> scrollValueChanged: aValue [

	self model scrollValue: aValue step
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> selectAll [
	self widgetDo: [ :w | w selectAll ]
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> selectFrom: nodeStart to: nodeStop [
	self widget selectFrom: nodeStart to: nodeStop
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> selectionInterval [
	^ self widget selectionInterval 
]

{ #category : #private }
MorphicTextAdapter >> setEditingModeFor: textArea [
	
	textArea beForPlainText
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> setHasUnacceptedEdits: aBoolean [

	self widgetDo: [ :w | w basicHasUnacceptedEdits: aBoolean ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> setScrollValue: aValue [
	self
		widgetDo: [ :w | 
			w scrollPane
				hScrollbarValue: aValue x;
				vScrollbarValue: aValue y;
				updateScrollbars ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> setSelection: interval [

	self model setSelectionInterval: interval
]

{ #category : #'spec protocol' }
MorphicTextAdapter >> setSelectionFromModel: aSelection [

	self widget ifNotNil: [:w | w setSelection: aSelection ]
]

{ #category : #'widget API' }
MorphicTextAdapter >> wantsVisualFeedback [

	^ self model wantsVisualFeedback
]
