Class {
	#name : #GLMMorphicSpecRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#instVars : [
		'container'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicSpecRenderer >> actOnPresentationUpdated: ann [
	container removeAllMorphs.
	container addMorphBack: (self morphFrom: ann presentation)
]

{ #category : #rendering }
GLMMorphicSpecRenderer >> render: aPresentation [
	container := GLMMorphic containerMorph.
	container
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	container addMorphBack: aPresentation displayValue widget widget.
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	^ container
]
