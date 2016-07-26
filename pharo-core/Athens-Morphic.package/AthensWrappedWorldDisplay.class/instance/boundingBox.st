display protocol
boundingBox
	^ wrapMorph ifNotNil: [ wrapMorph fullBounds ] ifNil: [ 0 @ 0 extent: 0 @ 0 ]