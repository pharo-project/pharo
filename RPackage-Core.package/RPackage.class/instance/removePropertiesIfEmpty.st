properties
removePropertiesIfEmpty
	^ Properties at: self ifPresent: [ :dict |
		dict ifEmpty: [ Properties removeKey: self ] ]