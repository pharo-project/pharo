Class {
	#name : #GLMMorphicPaneWithoutTitleRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'paneMorph'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicPaneWithoutTitleRenderer >> actOnMatchingPresentationsChanged: anAnnouncement [ 
	paneMorph	removeAllMorphs.
	self renderMorphFor: anAnnouncement pane.
	self renderer window fullBounds
]

{ #category : #rendering }
GLMMorphicPaneWithoutTitleRenderer >> render: aPane [
	paneMorph := GLMMorphic containerMorph.
	aPane when: GLMMatchingPresentationsChanged send: #actOnMatchingPresentationsChanged: to: self.
	self renderMorphFor: aPane.
	^ paneMorph
]

{ #category : #rendering }
GLMMorphicPaneWithoutTitleRenderer >> renderMorphFor: aPane [
	paneMorph
		addMorph: (self renderObject: aPane presentations) fullFrame: (LayoutFrame identity);
		"adoptPaneColor: paneMorph paneColor"
		yourself
]
