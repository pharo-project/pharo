"
I am a Glamour renderer that uses morphic in a external window.
"
Class {
	#name : #GLMOSWindowMorphWorldRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'window'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMOSWindowMorphWorldRenderer >> actOnPresentationUpdated: ann [
		self setLabelOf: window for: ann presentation
]

{ #category : #rendering }
GLMOSWindowMorphWorldRenderer >> handlePotentialStatusbarFor: aRenderable [
	| statusbarMorph bottomOffset |
	bottomOffset := 0.
	aRenderable hasStatusbar
		ifTrue: [ 
			statusbarMorph := GLMMorphicPaneWithoutTitleRenderer render: aRenderable statusbarPane from: self renderer.
			bottomOffset := (0 - (LabelMorph contents: 'XXX') minHeight) * 1.2.
			window
				addMorph: statusbarMorph
				fullFrame: (LayoutFrame identity
					topFraction: 1;
					leftOffset: 2;
					topOffset: bottomOffset;
					rightOffset: -2)].
	^ bottomOffset
]

{ #category : #rendering }
GLMOSWindowMorphWorldRenderer >> render: aRenderable [
	| dock offset bottomOffset |
	window := GLMOSWindowWorldMorph new model: aRenderable.
	self installActionsOnModel: window fromPresentation: aRenderable.
	self installKeystrokeActionsOnMorph: window fromPresentation: aRenderable.
	self setLabelOf: window for: aRenderable.
	offset := 4.
	bottomOffset := 0.
	dock := self renderToolbarForPresentation: aRenderable inMorph: window.
	dock hResizing: #shrinkWrap.
	window toolbarBox: dock.
	bottomOffset := self handlePotentialStatusbarFor: aRenderable.
	window
		addMorph: (self renderObject: aRenderable)
		fullFrame: ((0 @ 0 corner: 1 @ 1) asLayoutFrame 
						topLeftOffset: 0 @ offset ; 
						bottomRightOffset: 0@ bottomOffset).
	self setPotentialBackgroundColorFor: aRenderable.
	aRenderable when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	aRenderable when: GLMWindowRequest send: #actOnWindowRequest: to: window.
	aRenderable watcherPane notNil
		ifTrue: [ GLMMorphicWatcherRenderer render: aRenderable watcherPane from: self renderer ].
	^ window
]

{ #category : #rendering }
GLMOSWindowMorphWorldRenderer >> setLabelOf: aWindow for: aRenderable [
	"aWindow title: (self titleFor: aRenderable)."
]

{ #category : #rendering }
GLMOSWindowMorphWorldRenderer >> setPotentialBackgroundColorFor: aRenderable [
	aRenderable colorValue ifNotNil: [ window borderColor: aRenderable colorValue ]
]

{ #category : #rendering }
GLMOSWindowMorphWorldRenderer >> titleFor: aRenderable [
	^ aRenderable titleValue ifNil: [ 'Glamorous Browser' ]
]
