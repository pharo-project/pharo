title
removeTitle
	titleStringMorph
		ifNil: [^ self].
	self removeMorph: titleStringMorph owner. 
	titleStringMorph := nil