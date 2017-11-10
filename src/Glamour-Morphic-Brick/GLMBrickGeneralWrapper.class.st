Class {
	#name : #GLMBrickGeneralWrapper,
	#superclass : #Object,
	#instVars : [
		'brick'
	],
	#category : #Glamour-Morphic-Brick-Layouts-Utils
}

{ #category : #'instance creation' }
GLMBrickGeneralWrapper class >> on: aBrick [

	^ self new brick: aBrick
]

{ #category : #accessing }
GLMBrickGeneralWrapper >> brick [

	^ brick
]

{ #category : #accessing }
GLMBrickGeneralWrapper >> brick: aBrick [

	brick := aBrick
]

{ #category : #api }
GLMBrickGeneralWrapper >> changed [

	^ self brick changed
]

{ #category : #api }
GLMBrickGeneralWrapper >> hState [

	^ self layout hState
]

{ #category : #api }
GLMBrickGeneralWrapper >> isBrick [

	^ self brick isBrick
]

{ #category : #api }
GLMBrickGeneralWrapper >> layout [

	^ self brick layoutProperties
]

{ #category : #api }
GLMBrickGeneralWrapper >> layoutPolicy [

	^ self brick layoutPolicy
]

{ #category : #api }
GLMBrickGeneralWrapper >> owner [

	^ self brick owner ifNotNil: #brickApi
]

{ #category : #api }
GLMBrickGeneralWrapper >> subbricks [

	^ self brick subbricks collect:  #brickApi
]

{ #category : #api }
GLMBrickGeneralWrapper >> vState [

	^ self layout vState
]
