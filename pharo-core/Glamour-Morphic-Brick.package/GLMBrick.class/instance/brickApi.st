*Glamour-Morphic-Brick
brickApi

	^ brickApi ifNil: [ brickApi := GLMBrickWrapper on: self ]