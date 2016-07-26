analyzing
addVariableReferenceFrom: anObject to: anotherObject
	| list |
	list := self variableReferences
		at: anObject
		ifAbsent: [ nil ].
	list ifNil: [ self variableReferences at: anObject put: (list := OrderedCollection new) ].	
	list add: anotherObject