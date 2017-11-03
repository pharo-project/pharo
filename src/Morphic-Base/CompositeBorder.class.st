"
Border supporting multiple ""sub-borders"".
"
Class {
	#name : #CompositeBorder,
	#superclass : #SimpleBorder,
	#instVars : [
		'borders'
	],
	#category : #Morphic-Base-Borders
}

{ #category : #comparing }
CompositeBorder >> = aBorderStyle [
	"Check the sub-borders too"
	
	^super = aBorderStyle and: [
		self borders = aBorderStyle borders]
]

{ #category : #accessing }
CompositeBorder >> borders [
	"Answer the value of borders"

	^ borders
]

{ #category : #accessing }
CompositeBorder >> borders: anObject [
	"Set the value of borders"

	borders := anObject
]

{ #category : #accessing }
CompositeBorder >> colorsAtCorners [
	"Return the colors of the first border."
	
	^self borders first colorsAtCorners
]

{ #category : #drawing }
CompositeBorder >> frameRectangle: aRectangle on: aCanvas [
	"Draw each border in turn."
	
	|r|
	r := aRectangle.
	self borders do: [:b |
		b frameRectangle: r on: aCanvas.
		r := r insetBy: b width]
]

{ #category : #comparing }
CompositeBorder >> hash [
	"Since #= is overridden."
	
	^super hash bitXor: self borders hash
]

{ #category : #testing }
CompositeBorder >> isComposite [
	"Answer true."

	^true
]
