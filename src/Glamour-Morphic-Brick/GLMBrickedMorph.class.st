"
A GLMBrickedMorph is a direct subclass of Morph with removed layouting mechanism
"
Class {
	#name : #GLMBrickedMorph,
	#superclass : #Morph,
	#category : #'Glamour-Morphic-Brick-Morphic'
}

{ #category : #layout }
GLMBrickedMorph >> adjustLayoutBounds [
	"no need in it"
]

{ #category : #drawing }
GLMBrickedMorph >> clippingBounds [

	^ self fullBounds
]

{ #category : #layout }
GLMBrickedMorph >> computeBounds [
	"nothing to compute, everything is already computed"
]

{ #category : #layout }
GLMBrickedMorph >> computeFullBounds [
	"nothing to compute, everything is already computed"
]

{ #category : #layout }
GLMBrickedMorph >> doLayoutIn: layoutBounds [
	"we remove morphic layouting mechanism"
]

{ #category : #layout }
GLMBrickedMorph >> layoutBounds: aRectangle [
	"I'm not a morph anymore"
]

{ #category : #layout }
GLMBrickedMorph >> layoutChanged [

	"empty"
]

{ #category : #geometry }
GLMBrickedMorph >> outerBounds [
	^ self fullBounds
]

{ #category : #layout }
GLMBrickedMorph >> privateFullBounds [
	^ self fullBounds
]

{ #category : #'change reporting' }
GLMBrickedMorph >> privateInvalidateMorph: aMorph [
	"empty"
]
