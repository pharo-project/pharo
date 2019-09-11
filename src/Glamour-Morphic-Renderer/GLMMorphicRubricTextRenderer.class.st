Class {
	#name : #GLMMorphicRubricTextRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'textMorph',
		'textModel'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #private }
GLMMorphicRubricTextRenderer >> actOnContextChanged: ann [
	ann property = #selectionInterval
		ifTrue: [
			ann value notNil 
				ifTrue: [textMorph setSelection: ann value ]
				ifFalse: [textMorph setSelection: (1 to: 0)] ]
]

{ #category : #private }
GLMMorphicRubricTextRenderer >> actOnPresentationUpdated: ann [ 
	| anInterval |
	anInterval := ann presentation selectionInterval.
	textMorph setText: ann presentation formatedDisplayValue.
	textMorph setSelection: anInterval
]

{ #category : #private }
GLMMorphicRubricTextRenderer >> actOnTextEditorRequest: ann [

	ann action value: textMorph textArea editor 	

]

{ #category : #rendering }
GLMMorphicRubricTextRenderer >> modelFor: aPresentation [
	^ GLMRubricTextModel new
		glamourPresentation: aPresentation;
		yourself
]

{ #category : #rendering }
GLMMorphicRubricTextRenderer >> morph [
	^ RubScrolledTextMorph new
		getSelectionSelector: #primarySelectionInterval;
		color: self theme backgroundColor;
		textColor:
			self theme textColor;
		textFont: StandardFonts defaultFont;
		model: textModel;
		yourself
]

{ #category : #private }
GLMMorphicRubricTextRenderer >> registerEventsForPresentation: aPresentation [
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMTextEditorRequest send: #actOnTextEditorRequest: to: self
]

{ #category : #rendering }
GLMMorphicRubricTextRenderer >> render: aPresentation [
	textModel := self modelFor: aPresentation.
	textModel primarySelectionInterval: (aPresentation primarySelectionInterval 
		ifNil:[ aPresentation selectionInterval ifNil: [1 to: 0] ]).
	textMorph := self morph.
	aPresentation text: textMorph text.
	aPresentation withLineNumbers
		ifTrue: [ textMorph withLineNumbers ]
		ifFalse: [ textMorph withoutLineNumbers ].
	aPresentation withAnnotation
		ifTrue: [ textMorph withAnnotation ]
		ifFalse: [ textMorph withoutAnnotation ].
	aPresentation withColumns 
		ifTrue: [ textMorph withColumns ]
		ifFalse: [ textMorph withoutColumns ].
	textMorph wrapped: aPresentation wrapped.
	textMorph tabWidth: aPresentation tabWidth.
	aPresentation withGhostText ifNotNil: [ 
		textMorph withGhostText: aPresentation withGhostText asStringOrText ].
	aPresentation textSegments do: [:each | 
			textMorph addSegment: each ].
	(aPresentation textSegments anySatisfy: [ :ts | ts icon notNil ])
		ifTrue: [ textMorph withTextSegmentIcons ]
		ifFalse: [ textMorph withoutTextSegmentIcons ].
	textMorph textArea announcer
		when: RubTextChanged
		do: [ :ann | 
			aPresentation announcer
				suspendAllWhile: [ aPresentation text: textMorph text ] ].
	textMorph textArea announcer
		when: RubSelectionChanged
		do: [ :ann | 
			aPresentation announcer 
				suspendAllWhile: [ 
					aPresentation selectionInterval: textMorph selectionInterval.
					aPresentation selectedText: textMorph textArea selection ] ].
			
	textMorph textArea announcer
		when: RubTextAccepted
		do: [ :ann | aPresentation suspendAllWhile: [ aPresentation text: ann text ] ].
		
	self installActionsOnModel: textModel fromPresentation: aPresentation .
	self installKeystrokeActionsOnMorph: textMorph textArea fromPresentation: aPresentation.
	
	self registerEventsForPresentation: aPresentation.
	
	^ textMorph
]
