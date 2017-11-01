Class {
	#name : #RubScrolledTextMorphThemerForGLMWhitespaceTheme,
	#superclass : #RubScrolledTextMorphThemer,
	#category : 'Glamour-Morphic-Theme'
}

{ #category : #border-styles }
RubScrolledTextMorphThemerForGLMWhitespaceTheme >> normalBorderStyleFor: aTextArea [
	^ BorderStyle 
		width: 1
		color: (GLMWhitespaceTheme lightBaseColor)
]
