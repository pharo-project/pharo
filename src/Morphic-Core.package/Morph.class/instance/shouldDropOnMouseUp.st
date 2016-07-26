testing
shouldDropOnMouseUp
	| former |
	former := self formerPosition ifNil:[^false].
	^(former dist: self position) > 10