rubric-accessing
rubric: aMorph

	self morph ifNotNil: [ self removeMorph: self morph ].
	self on: aMorph