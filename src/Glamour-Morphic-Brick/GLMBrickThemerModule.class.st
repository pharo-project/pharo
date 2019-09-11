Class {
	#name : #GLMBrickThemerModule,
	#superclass : #Object,
	#instVars : [
		'themer'
	],
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #values }
GLMBrickThemerModule >> theme [ 
	^ Smalltalk ui theme
]

{ #category : #accessing }
GLMBrickThemerModule >> themer [

	^ themer
]

{ #category : #accessing }
GLMBrickThemerModule >> themer: anObject [
	themer := anObject
]
