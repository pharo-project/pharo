accessing-properties
removePropertiesIfEmpty
	^ TraitProperties at: self ifPresent: [ :dict |
		dict ifEmpty: [ TraitProperties removeKey: self ] ]
		