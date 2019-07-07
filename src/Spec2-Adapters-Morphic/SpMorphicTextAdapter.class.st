"
I am the adapter used to bridge a TextPresenter and a PluggableTextMorph
"
Class {
	#name : #SpMorphicTextAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#traits : 'TViewModel',
	#classTraits : 'TViewModel classTrait',
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> accept [
	"We use RubScrolledTextMorph>>#acceptContents but we keep accept as method name for compatibility"
	self widgetDo: [ :w | w acceptContents ]
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> accept: aText notifying: aNotifyier [

	self model accept: aText notifying: aNotifyier
]

{ #category : #accessing }
SpMorphicTextAdapter >> acceptAllowed [
	^self widget editingMode acceptAllowed
]

{ #category : #accessing }
SpMorphicTextAdapter >> announcer [
	^ self model announcer

	
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> askBeforeDiscardingEdits [

	^ self model askBeforeDiscardingEdits
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> askBeforeDiscardingEdits: aBoolean [
	self widget askBeforeDiscardingEdits: aBoolean
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> autoAccept [

	^ self model autoAccept
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> autoAccept: aBoolean [

	^ self model autoAccept: aBoolean
]

{ #category : #factory }
SpMorphicTextAdapter >> buildWidget [
	| newWidget |
	self flag: #pharo7.	"When support for P7 will be dropped, remove this compatibility hack."
	newWidget := ((self class environment at: #SpRubScrolledTextMorph ifAbsent: [ RubScrolledTextMorph ]) on: self)
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
SpMorphicTextAdapter >> canChangeText [
	^ self widget textArea canChangeText
]

{ #category : #'undo-redo' }
SpMorphicTextAdapter >> clearUndoManager [
	self widget textArea editingState clearUndoManager: nil
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> clearUserEditFlag [

	self model text: self getText.
	self changed: #clearUserEdits
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> codePaneMenu: aMenu shifted: shifted [ 
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpBindings
		value: self model application adapterBindings
		during: [ menuPresenter value buildWithSpec ]
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> font: aFont [
	self widgetDo: [ :w | w font: aFont ]
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> getText [

	^ self model text
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> hasEditingConflicts: aBoolean [
 
	self widgetDo: [ :w | w hasEditingConflicts: aBoolean ]
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> hasUnacceptedEdits: aBoolean [

	self model hasUnacceptedEdits: aBoolean
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> notify: errorMessage at: position in: sourceCode [

	self flag: #TOMOVE.
	self widgetDo: [ :w | w notify: errorMessage at: position in: sourceCode ]
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> pendingText [

	^ self widgetDo: [ :w | w text ]
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> pendingText: aText [

	self widgetDo: [ :w | 
		w setText: aText.
		w hasUnacceptedEdits: true ]
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> readSelection [

	^ self model readSelection
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> scrollValueChanged: aValue [

	self model scrollValue: aValue step
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> selectAll [
	self widgetDo: [ :w | w selectAll ]
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> selectFrom: nodeStart to: nodeStop [
	self widget selectFrom: nodeStart to: nodeStop
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> selectionInterval [
	^ self widget selectionInterval 
]

{ #category : #private }
SpMorphicTextAdapter >> setEditingModeFor: textArea [
	
	textArea beForPlainText
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> setHasUnacceptedEdits: aBoolean [

	self widgetDo: [ :w | w basicHasUnacceptedEdits: aBoolean ]
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> setScrollValue: aValue [
	self
		widgetDo: [ :w | 
			w scrollPane
				hScrollbarValue: aValue x;
				vScrollbarValue: aValue y;
				updateScrollbars ]
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> setSelection: interval [

	self model setSelectionInterval: interval
]

{ #category : #'spec protocol' }
SpMorphicTextAdapter >> setSelectionFromModel: aSelection [

	self widget ifNotNil: [:w | w setSelection: aSelection ]
]

{ #category : #'widget API' }
SpMorphicTextAdapter >> wantsVisualFeedback [

	^ self model wantsVisualFeedback
]
