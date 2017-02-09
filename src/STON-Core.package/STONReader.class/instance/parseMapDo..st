parsing
parseMapDo: block
	self expectChar: ${.
	(self matchChar: $}) ifTrue: [ ^ self ]. "short cut for empty maps"
	[ readStream atEnd ] whileFalse: [ | name value |
		name := self parseSimpleValue.
		self expectChar: $:.
		value := self parseValue.
		block value: name value: value.
		"The above is a more efficient way to say 'self parseValue' and using the returned association"
		(self matchChar: $}) ifTrue: [ ^ self ].
		self expectChar: $, ].
	self error: 'end of map expected'