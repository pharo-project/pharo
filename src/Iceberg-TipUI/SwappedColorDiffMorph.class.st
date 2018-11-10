"
I swap colors for the original diff morph, so the additions are green and removals are red.
(and order left to right is preserved)
"
Class {
	#name : #SwappedColorDiffMorph,
	#superclass : #DiffMorph,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #'accessing colors' }
SwappedColorDiffMorph >> additionColor [
	^ super removalColor

]

{ #category : #'accessing colors' }
SwappedColorDiffMorph >> additionHighlightColor [
	^ super removalHighlightColor

]

{ #category : #'accessing colors' }
SwappedColorDiffMorph >> removalColor [

	^ super additionColor 
]

{ #category : #'accessing colors' }
SwappedColorDiffMorph >> removalHighlightColor [
	^ super additionHighlightColor

]
