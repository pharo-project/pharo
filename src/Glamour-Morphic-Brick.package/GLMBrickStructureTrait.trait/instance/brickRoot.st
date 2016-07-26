brick-structure
brickRoot

	^ self owner ifNotNil: [ self owner brickRoot ] ifNil: [ ^ self ]