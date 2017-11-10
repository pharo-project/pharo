"
This renders the top most browser or composite presentation in a window.
"
Class {
	#name : #GLMMorphicWindowRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'window'
	],
	#category : #Glamour-Morphic-Renderer
}

{ #category : #rendering }
GLMMorphicWindowRenderer >> actOnPresentationToolbarUpdated: ann [
	| dock |

	self setLabelOf: window for: ann presentation.
	dock := self renderToolbarForPresentation: ann presentation inMorph: window.
	dock hResizing: #shrinkWrap.
	window toolbarBox: dock.
]

{ #category : #rendering }
GLMMorphicWindowRenderer >> actOnPresentationUpdated: ann [
		self setLabelOf: window for: ann presentation
]

{ #category : #rendering }
GLMMorphicWindowRenderer >> handlePotentialStatusbarFor: aRenderable [
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
GLMMorphicWindowRenderer >> render: aRenderable [
	| dock offset bottomOffset |
	window := GLMSystemWindow new model: aRenderable.
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
	aRenderable when: GLMPresentationToolbarUpdated send: #actOnPresentationToolbarUpdated: to: self.
	aRenderable when: GLMWindowRequest send: #actOnWindowRequest: to: window.
	aRenderable watcherPane notNil
		ifTrue: [ GLMMorphicWatcherRenderer render: aRenderable watcherPane from: self renderer ].
	^ window
]

{ #category : #rendering }
GLMMorphicWindowRenderer >> renderToolbarForPresentation: aPresentation inMorph: aMorph [
	| dock buttons |
	dock := aMorph theme newToolDockingBarIn: aMorph.
	buttons := self toolbarButtonsForPresentation: aPresentation.
	dock addAllMorphs: buttons.
	dock addMorphBack: (self 
		dropDownMenuFrom: [ | menu defaultMenu |
				defaultMenu := aMorph buildWindowMenu.
				(aPresentation allActions select: #isMenu) ifNotEmpty: [ 
					defaultMenu addLine ].
				menu := self menuIn: defaultMenu for: aPresentation.
				menu popUpEvent: self currentEvent in: aMorph world ]
		withIcon: aMorph theme windowMenuForm).
	^ dock
]

{ #category : #rendering }
GLMMorphicWindowRenderer >> setLabelOf: aWindow for: aRenderable [
	aWindow setLabel: (self titleFor: aRenderable).
]

{ #category : #rendering }
GLMMorphicWindowRenderer >> setPotentialBackgroundColorFor: aRenderable [
	aRenderable colorValue ifNotNil: [ window borderColor: aRenderable colorValue ]
]

{ #category : #rendering }
GLMMorphicWindowRenderer >> titleFor: aRenderable [
	^ aRenderable titleValue ifNil: [ 'Glamorous Browser' ]
]
