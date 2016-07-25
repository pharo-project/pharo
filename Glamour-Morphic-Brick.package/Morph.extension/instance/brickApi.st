*Glamour-Morphic-Brick
brickApi

	^ self valueOfProperty: #brickApi ifAbsentPut: [ GLMBrickMorphWrapper on: self ]