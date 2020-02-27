Class {
	#name : #SpMorphicBaseTextAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#traits : 'TViewModel',
	#classTraits : 'TViewModel classTrait',
	#instVars : [
		'selection'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> accept [
	"We use RubScrolledTextMorph>>#acceptContents but we keep accept as method name for compatibility"
	self widgetDo: [ :w | w acceptContents ]
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> accept: aText notifying: aNotifyier [
	| accept |

	accept := self presenter acceptBlock 
		cull: aText 
		cull: aNotifyier.
	accept == false ifTrue: [ ^ false ].

	self presenter text: aText copy.
	"Raise an announcement"
	self presenter actionPerformed.
	self presenter changed: #clearUserEdits.
	
	^ true
]

{ #category : #accessing }
SpMorphicBaseTextAdapter >> acceptAllowed [
	^self widget editingMode acceptAllowed
]

{ #category : #accessing }
SpMorphicBaseTextAdapter >> announcer [
	^ self model announcer

	
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> askBeforeDiscardingEdits [

	^ self model askBeforeDiscardingEdits
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> askBeforeDiscardingEdits: aBoolean [
	self widget askBeforeDiscardingEdits: aBoolean
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> autoAccept [

	^ self model autoAccept
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> autoAccept: aBoolean [

	^ self model autoAccept: aBoolean
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> canChangeText [
	^ self widget textArea canChangeText
]

{ #category : #'undo-redo' }
SpMorphicBaseTextAdapter >> clearUndoManager [

	self widget textArea editingState clearUndoManager: nil
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> clearUserEditFlag [

	self model text: self getText.
	self changed: #clearUserEdits
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> codePaneMenu: aMenu shifted: shifted [
	| menuPresenter |

	menuPresenter := self model contextMenu.
	menuPresenter ifNil: [ ^ nil ].
	^ SpBindings
		value: self model application adapterBindings
		during: [ menuPresenter value buildWithSpec ]
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> font: aFont [
	self widgetDo: [ :w | w font: aFont ]
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> getText [

	^ self model text
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> hasEditingConflicts: aBoolean [
 
	self widgetDo: [ :w | w hasEditingConflicts: aBoolean ]
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> hasUnacceptedEdits: aBoolean [

	self model hasUnacceptedEdits: aBoolean
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> insert: aString at: positionIndex [
	| text |

	text := self getText.
	text := 
		(text copyFrom: 1 to: positionIndex), 
		aString, 
		(text copyFrom: positionIndex + 1 to: text size).

	self model text: text
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> notify: errorMessage at: position in: sourceCode [

	self flag: #TOMOVE.
	self widgetDo: [ :w | w notify: errorMessage at: position in: sourceCode ]
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> pendingText [

	^ self widgetDo: [ :w | w text ]
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> pendingText: aText [

	self widgetDo: [ :w | 
		w setText: aText.
		w hasUnacceptedEdits: true ]
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> placeholder [
	^ self model placeholder
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> placeholder: aText [
	^ self model placeholder: aText
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> readSelection [

	^ self model readSelection
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> scrollValueChanged: aValue [

	self model scrollValue: aValue step
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> selectAll [
	self widgetDo: [ :w | w selectAll ]
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> selectFrom: nodeStart to: nodeStop [
	self widget selectFrom: nodeStart to: nodeStop
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> selectLine [

	self widgetDo: [ :w |
		w textArea editor selectLine ]

]

{ #category : #accessing }
SpMorphicBaseTextAdapter >> selection [
	
	^ selection
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> selectionInterval [
	^ self widget selectionInterval 
]

{ #category : #private }
SpMorphicBaseTextAdapter >> setEditingModeFor: textArea [
	
	textArea beForPlainText
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> setHasUnacceptedEdits: aBoolean [

	self widgetDo: [ :w | w basicHasUnacceptedEdits: aBoolean ]
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> setScrollValue: aValue [
	self
		widgetDo: [ :w | 
			w scrollPane
				hScrollbarValue: aValue x;
				vScrollbarValue: aValue y;
				updateScrollbars ]
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> setSelection: interval [

	selection := interval
]

{ #category : #'spec protocol' }
SpMorphicBaseTextAdapter >> setSelectionFromModel: aSelection [

	self widget ifNotNil: [ :w | w setSelection: aSelection ]
]

{ #category : #'widget API' }
SpMorphicBaseTextAdapter >> wantsVisualFeedback [

	^ self model wantsVisualFeedback
]
