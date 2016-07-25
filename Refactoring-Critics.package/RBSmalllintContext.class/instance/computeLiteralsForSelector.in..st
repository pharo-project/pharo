private
computeLiteralsForSelector: aSelector in: aClass 
	| method |
	method := aClass compiledMethodAt: aSelector ifAbsent: [nil].
	method isNil ifTrue: [^self].
	self addLiteralsFor: method