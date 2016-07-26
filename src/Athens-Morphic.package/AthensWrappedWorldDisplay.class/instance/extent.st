display protocol
extent
	^ wrapMorph ifNotNil: [ wrapMorph extent ] ifNil: [ 0 @ 0 ]