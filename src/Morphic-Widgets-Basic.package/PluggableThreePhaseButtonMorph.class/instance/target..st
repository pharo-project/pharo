accessing
target: anObject

	target ifNotNil: [ target removeDependent: self ].
	anObject ifNotNil: [ anObject addDependent: self ].
	target := anObject.