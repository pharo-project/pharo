Class {
	#name : #GLMRubTextScrollPane,
	#superclass : #RubTextScrollPane,
	#category : 'Glamour-Morphic-Brick-Morphic'
}

{ #category : #drawing }
GLMRubTextScrollPane >> drawKeyboardFocusOn: aCanvas [
	"empty to remove focus border"
]

{ #category : #drawing }
GLMRubTextScrollPane >> drawKeyboardFocusOnAthensCanvas: aCanvas [
	"empty to remove focus border"
]

{ #category : #scrollbars }
GLMRubTextScrollPane >> hScrollbarInterval [
	"Answer the computed size of the thumb of the horizontal scrollbar."
	self scrollTarget width = 0 ifTrue: [ ^ 0 ].
	^ self scrollBounds width asFloat / self scrollTarget width min: 1.0.
]
