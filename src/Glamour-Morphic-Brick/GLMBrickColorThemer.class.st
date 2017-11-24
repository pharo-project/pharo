"
I define different usefull colors
"
Class {
	#name : #GLMBrickColorThemer,
	#superclass : #GLMBrickThemerModule,
	#category : #'Glamour-Morphic-Brick-Theme'
}

{ #category : #'brick-colors' }
GLMBrickColorThemer >> alizarin [

	^ Color brickColorRegistry at: #brickAlizarin
		ifAbsentPut: [ Color r: 231 g: 76 b: 60 range: 255 ]
]

{ #category : #'brick-colors' }
GLMBrickColorThemer >> clouds [

	^ Color brickColorRegistry at: #brickClouds
		ifAbsentPut: [ Color r: 236 g: 240 b: 241 range: 255 ]
]

{ #category : #'brick-colors' }
GLMBrickColorThemer >> darken [

	^ Color brickColorRegistry at: #brickDarken
		ifAbsentPut: [ Color r: 29 g: 29 b: 29 range: 255 ]
]

{ #category : #'brick-colors' }
GLMBrickColorThemer >> emerald [

	^ Color brickColorRegistry at: #brickEmerald
		ifAbsentPut: [ Color r: 46 g: 204 b: 113 range: 255 ]
]

{ #category : #'brick-colors' }
GLMBrickColorThemer >> peterRiver [

	^ Color brickColorRegistry at: #brickPeterRiver
		ifAbsentPut: [ Color r: 52 g: 152 b: 219 range: 255 ]
]

{ #category : #'brick-colors' }
GLMBrickColorThemer >> silver [

	^ Color brickColorRegistry at: #brickSilver
		ifAbsentPut: [ Color r: 189 g: 195 b: 199 range: 255 ]
]

{ #category : #'brick-colors' }
GLMBrickColorThemer >> whiten [

	^ Color brickColorRegistry at: #brickWhiten
		ifAbsentPut: [ Color r: 241 g: 241 b: 241 range: 255 ]
]
