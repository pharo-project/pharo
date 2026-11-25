Class {
	#name : 'RubPlainSegmentMorph',
	#superclass : 'RubTextSegmentMorph',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'initialization' }
RubPlainSegmentMorph >> defaultBorderColor [
	^ Color yellow
]

{ #category : 'initialization' }
RubPlainSegmentMorph >> defaultColor [
	^ Color yellow alpha: 0.5
]
