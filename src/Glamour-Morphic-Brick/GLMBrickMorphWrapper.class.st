Class {
	#name : #GLMBrickMorphWrapper,
	#superclass : #GLMBrickGeneralWrapper,
	#category : #'Glamour-Morphic-Brick-Layouts-Utils'
}

{ #category : #api }
GLMBrickMorphWrapper >> hState [

	^ GLMBrickLayoutDummState new
]

{ #category : #api }
GLMBrickMorphWrapper >> isBrick [

	^ false
]

{ #category : #api }
GLMBrickMorphWrapper >> subbricks [

	^ self brick submorphs
]

{ #category : #api }
GLMBrickMorphWrapper >> vState [

	^ GLMBrickLayoutDummState new
]
