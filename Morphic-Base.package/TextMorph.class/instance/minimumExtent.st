geometry
minimumExtent


	| minExt |
	textStyle ifNil: [^ 9@16].
	borderWidth ifNil: [^ 9@16].
	minExt := (9@(textStyle lineGrid+2)) + (borderWidth*2).
	^ ((0@0 extent: minExt) expandBy: margins) extent