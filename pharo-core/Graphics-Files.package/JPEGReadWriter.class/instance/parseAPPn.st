marker parsing
parseAPPn
	| length buffer thumbnailLength markerStart |
	markerStart := self position.
	length := self nextWord.
	buffer := self next: 4.
	buffer asString = 'JFIF' ifFalse: 
		[ "Skip APPs that we're not interested in"
		stream next: length - 6.
		^ self ].
	self next.
	majorVersion := self next.
	minorVersion := self next.
	densityUnit := self next.
	xDensity := self nextWord.
	yDensity := self nextWord.
	thumbnailLength := self next * self next * 3.
	length := length - (self position - markerStart).
	length = thumbnailLength ifFalse: [ self error: 'APP0 thumbnail length is incorrect.' ].
	self next: length