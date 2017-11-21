Class {
	#name : #GLMMorphicVerticallyStackedRenderer,
	#superclass : #GLMMorphicCompositeRenderer,
	#category : #Glamour-Morphic-Renderer
}

{ #category : #rendering }
GLMMorphicVerticallyStackedRenderer >> render: aPresentation [
	| presentations container |
	presentations := aPresentation matchingPresentations.
	presentations isEmpty ifTrue: [ ^ GLMMorphic emptyMorph ].	
	container := GLMMorphic containerMorph.
	container changeTableLayout.
	presentations do: [ :each | 
		container addMorphBack: (self renderWithTitle: each) ].
	^ container
]
