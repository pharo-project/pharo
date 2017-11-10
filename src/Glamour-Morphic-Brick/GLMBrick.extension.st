Extension { #name : #GLMBrick }

{ #category : #'*Glamour-Morphic-Brick' }
GLMBrick >> brickApi [

	^ brickApi ifNil: [ brickApi := GLMBrickWrapper on: self ]
]
