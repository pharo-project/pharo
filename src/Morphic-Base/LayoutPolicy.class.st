"
A LayoutPolicy defines how submorphs of some morph should be arranged. Subclasses of the receiver define concrete layout policies.
"
Class {
	#name : #LayoutPolicy,
	#superclass : #Object,
	#category : 'Morphic-Base-Layouts'
}

{ #category : #layout }
LayoutPolicy >> flushLayoutCache [
	"Flush any cached information associated with the receiver"
]

{ #category : #utilities }
LayoutPolicy >> indexForInserting: aMorph at: aPoint in: someMorph [
	"Return the insertion index based on the layout strategy defined for some morph. Used for drop insertion."
	^1 "front-most"
]

{ #category : #testing }
LayoutPolicy >> isProportionalLayout [
	^false
]

{ #category : #testing }
LayoutPolicy >> isTableLayout [
	^false
]

{ #category : #layout }
LayoutPolicy >> layout: aMorph in: newBounds [
	"Compute the layout for the given morph based on the new bounds"

]

{ #category : #layout }
LayoutPolicy >> minExtentOf: aMorph in: newBounds [
	"Return the minimal size aMorph's children would require given the new bounds"
	^0@0
]
