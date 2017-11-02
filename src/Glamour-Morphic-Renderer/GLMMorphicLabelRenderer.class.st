Class {
	#name : #GLMMorphicLabelRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'labelMorph'
	],
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicLabelRenderer >> actOnPresentationUpdated: anAnnouncement [
	labelMorph contents: anAnnouncement presentation formatedDisplayValue
]

{ #category : #rendering }
GLMMorphicLabelRenderer >> render: aLabelPresentation [
	labelMorph := (LabelMorph contents: aLabelPresentation formatedDisplayValue)
		layoutFrame: (LayoutFrame identity);
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		font: StandardFonts haloFont;
		yourself.
	aLabelPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	^ labelMorph
]
