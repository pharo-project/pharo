Class {
	#name : #GLMMorphicFixedPanesFinderRenderer,
	#superclass : #GLMMorphicFinderRenderer,
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicFixedPanesFinderRenderer >> actOnPaneAdded: ann [
	container pushPane: (self renderObject: ann pane)
]

{ #category : #rendering }
GLMMorphicFixedPanesFinderRenderer >> actOnPaneRemoved: ann [
	container popPane
]

{ #category : #rendering }
GLMMorphicFixedPanesFinderRenderer >> actOnPaneReplaced: ann [
	container popAndReplacePane: (self renderObject: ann newPane)
]

{ #category : #rendering }
GLMMorphicFixedPanesFinderRenderer >> render: aBrowser [
	container := GLMPaneScroller new
		name: 'scroller';
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself.
	container maxPanes: aBrowser fixedSizePanes.
	self registerAnnouncementsFor: aBrowser.  
	aBrowser panes
		do: [ :each | container pushPane: (self renderObject: each) ].
	^ container
]
