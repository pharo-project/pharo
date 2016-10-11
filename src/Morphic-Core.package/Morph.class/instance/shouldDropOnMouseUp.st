testing
shouldDropOnMouseUp
	| former |
	former := self formerPosition ifNil:[^false].
	^(former distanceTo: self position) > 10