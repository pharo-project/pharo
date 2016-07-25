parsing
parseObject
	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}')
		ifTrue: [ ^ result ].
	[ stream atEnd ] whileFalse: [
		result := self
			addProperty: self parseProperty
			to: result.
		(self match: '}')
			ifTrue: [ ^ result ].
		self expect: ',' ].
	self error: 'end of object expected'