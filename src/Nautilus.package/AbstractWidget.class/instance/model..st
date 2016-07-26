accessing
model: anObject

	model ifNotNil: [  model removeDependent: self].
	anObject ifNotNil: [  anObject addDependent:self].
	model := anObject.
