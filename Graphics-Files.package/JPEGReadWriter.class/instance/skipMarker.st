marker parsing
skipMarker
	| length markerStart |
	markerStart := self position.
	length := self nextWord.
	self next: length - (self position - markerStart)