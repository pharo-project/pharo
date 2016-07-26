parsing
parseListDo: block
	| index |
	self expectChar: $[.
	(self matchChar: $]) ifTrue: [ ^ self ]. "short cut for empty lists"
	index := 1.
	[ readStream atEnd ] whileFalse: [
		block cull: self parseValue cull: index.
		(self matchChar: $]) ifTrue: [ ^ self ].
		index := index + 1.
		self expectChar: $, ].
	self error: 'end of list expected'