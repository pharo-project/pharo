protocol
compiled: selector in: class date: date

	| method |
	self addEntry: selector date: date.
	self class classes addEntry: class theNonMetaClass name date: date.
	(class canUnderstand: selector) ifFalse: [ ^self ].
	method := class compiledMethodAt: selector ifAbsent: [ ^self ].
	method messages do: [ :m | self  addEntry: m date: date ].
	method literals do: [ :each |
		(each isVariableBinding and: [
			each key notNil and: [
				each key first isUppercase ] ])
					ifTrue: [ 
						self class classes addEntry: each key date: date ] ]