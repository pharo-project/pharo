accessing-properties
removePropertiesIfEmpty
	^ ClassProperties at: self ifPresent: [ :dict |
		dict ifEmpty: [ ClassProperties removeKey: self ] ]
		