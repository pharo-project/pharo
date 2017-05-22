accessing
symbol
	<todo: 'now we can handle enums which define the same value multiple times, but in case of this handler, we don''t know which name we need to associate with the return value. This is basically random...'>
	^ self class declaration
		keyAtValue: self value
		ifAbsent: [ nil ]