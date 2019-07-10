Class {
	#name : #GLMMorphicRubricHighlightedTextRenderer,
	#superclass : #GLMMorphicRubricTextRenderer,
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicRubricHighlightedTextRenderer >> morph [
	| morph |
	morph := super morph.
	morph editingMode: textModel glamourPresentation editingMode.
	morph textFont: textModel glamourPresentation morphicFont.
	"Changing the editing mode makes the editor dirty."
	morph hasUnacceptedEdits: false.
	^ morph
		
]
