rulers handling
withoutRulerNamed: aKey
	(self rulerNamed: aKey)
		ifNotNil: [ :m | self withoutRuler: m ].
	self changed