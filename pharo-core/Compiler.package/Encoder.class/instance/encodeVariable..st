encoding
encodeVariable: name
	^ self encodeVariable: name sourceRange: nil ifUnknown: [ self undeclared: name ]