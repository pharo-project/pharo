testing
matchesTypes: types
	(self type isNil or: [types isNil])
		ifTrue: [^false].
	^types anySatisfy: [:mimeType | mimeType beginsWith: self type]