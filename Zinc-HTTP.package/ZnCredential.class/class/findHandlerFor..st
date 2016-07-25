accessing class hierarchy
findHandlerFor: anAuthHeader
	| type i |
	i := anAuthHeader indexOf: Character space.
	type := anAuthHeader copyFrom: 1 to: i - 1.
	self allSubclassesDo: [ :subclass |
		subclass authName = type ifTrue: [ ^ subclass ] ].
	^ nil