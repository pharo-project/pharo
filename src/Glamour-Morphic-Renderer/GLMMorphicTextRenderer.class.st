Class {
	#name : #GLMMorphicTextRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'textModel',
		'textMorph'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicTextRenderer >> actOnContextChanged: ann [ 
	ann property = #selectionInterval
		ifTrue: [ 
			ann value notNil 
				ifTrue: [textMorph setSelection: ann value ]
				ifFalse: [textMorph setSelection: (1 to: 0)] ].
"	self flag: 'somehow this disturbs copy paste and cmd+shift+left selection'.
	ann property = #text
		ifTrue: [ 
			textMorph announcer suspendAll: {GLMTextChanged} while: [ 
					textModel changedText: ann value ] ]"
]

{ #category : #rendering }
GLMMorphicTextRenderer >> actOnPresentationUpdated: ann [ 
	textModel text: ann presentation formatedDisplayValue.
	textModel changed: #text
]

{ #category : #rendering }
GLMMorphicTextRenderer >> registerEventsForPresentation: aPresentation [
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMSelectCurrentLine send: #selectLine to: self
]

{ #category : #rendering }
GLMMorphicTextRenderer >> render: aPresentation [
	textModel := self textModelFor: aPresentation.
	textMorph := self textMorphForModel: textModel.	"this is a horrible hack: 
	because we cannot get the update of when the text changes from the morph,
	we get the reference to the mutable text object :) :)"
	textModel text: textMorph text.
	textMorph setSelection: textModel selection.	"Without setting the selection again 
	the smalltalkcode presentation shows no selection. Seems to be cause by the font: 
	message send when creating a GLMPluggableTextMorph"
	self installActionsOnModel: textModel fromPresentation: aPresentation.
	self installKeystrokeActionsOnMorph: textMorph textMorph fromPresentation: aPresentation.
	textMorph announcer
		when: GLMSelectedTextChanged
		do: [ :ann | 
			aPresentation announcer
				suspendAllWhile: [ 
					aPresentation selectionInterval: ann interval.
					aPresentation selectedText: ann selectedText ] ].
	textMorph announcer when: GLMTextChanged do: [ :ann | aPresentation suspendAllWhile: [ aPresentation text: ann text ] ].
	self registerEventsForPresentation: aPresentation.
	^ textMorph
]

{ #category : #rendering }
GLMMorphicTextRenderer >> selectLine [
	
	textMorph selectLine
]

{ #category : #rendering }
GLMMorphicTextRenderer >> textModelFor: aPresentation [
	^ GLMTextModel new
		text: aPresentation formatedDisplayValue;
		glamourPresentation: aPresentation
]

{ #category : #rendering }
GLMMorphicTextRenderer >> textMorphForModel:  aTextModel [
	^ (RubPluggableTextMorph
		on: aTextModel
		text: #text
		accept: #accept:notifying:
		readSelection: #selection
		menu: #menu:shifted:)
		changedAction: [:aText|
			aText asString trimBoth ifNotEmpty:  [aTextModel text: aText] ];
		onKeyStrokeSend: #keystroke:from: to: aTextModel;
		layoutFrame: (LayoutFrame identity);
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		color: self theme backgroundColor;
		detachAllKeymapCategories;
		yourself
]
