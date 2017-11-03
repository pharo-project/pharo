"
A node for ""lazy drawing"" in a morphic based tree
"
Class {
	#name : #LazyMorphTreeNodeMorph,
	#superclass : #MorphTreeNodeMorph,
	#instVars : [
		'drawable'
	],
	#category : #Morphic-Widgets-Tree
}

{ #category : #accessing }
LazyMorphTreeNodeMorph >> drawOn: aCanvas [
	drawable
		ifFalse: [container buildRowMorphsFrom: self]
		ifTrue: [super drawOn: aCanvas]

]

{ #category : #accessing }
LazyMorphTreeNodeMorph >> drawSubmorphsOn: aCanvas [
	drawable ifFalse: [^ self].
	super drawSubmorphsOn: aCanvas.

]

{ #category : #drawing }
LazyMorphTreeNodeMorph >> drawable [
	^ drawable ifNil: [false]

]

{ #category : #drawing }
LazyMorphTreeNodeMorph >> drawable: aBoolean [
	drawable := aBoolean.

]

{ #category : #initialization }
LazyMorphTreeNodeMorph >> initRow [
]

{ #category : #initialization }
LazyMorphTreeNodeMorph >> initialize [
	super initialize.
	drawable := false
]
