Class {
	#name : #RubPlainSegmentMorph,
	#superclass : #RubTextSegmentMorph,
	#category : 'Rubric-Editing-Core'
}

{ #category : #initialization }
RubPlainSegmentMorph >> defaultBorderColor [
	^ Color yellow

]

{ #category : #initialization }
RubPlainSegmentMorph >> defaultColor [
	^ Color yellow alpha: 0.5.

]
