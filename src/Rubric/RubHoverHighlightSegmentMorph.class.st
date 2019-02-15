"
I am a text segment morph used for highlighting a text segment on mouse over. 
My default color are the text secondary (light) color.
And my default behavior is to delete myself when the mouse focus leaves me.
"
Class {
	#name : #RubHoverHighlightSegmentMorph,
	#superclass : #RubTextSegmentMorph,
	#category : #'Rubric-Editing-Core'
}

{ #category : #initialization }
RubHoverHighlightSegmentMorph >> defaultBorderColor [
	^ self defaultColor muchDarker

]

{ #category : #initialization }
RubHoverHighlightSegmentMorph >> defaultColor [
	^ self theme secondarySelectionColor
]

{ #category : #initialization }
RubHoverHighlightSegmentMorph >> initialize [
	super initialize.
	self borderWidth: 1.
	self borderColor: self defaultBorderColor
]

{ #category : #'event handling' }
RubHoverHighlightSegmentMorph >> mouseLeave: anEvent [
	super mouseLeave: anEvent.
	self delete.
]
