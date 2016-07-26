accessing-properties
removeProperty: propName
	^ self
		removeProperty: propName
		ifAbsent: [ nil ]