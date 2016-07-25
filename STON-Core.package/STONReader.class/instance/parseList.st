parsing
parseList
	| reference array |
	reference := self newReference.
	array := STON listClass streamContents: [ :stream |
		self parseListDo: [ :each | stream nextPut: each ] ].
	self setReference: reference to: array.
	^ array