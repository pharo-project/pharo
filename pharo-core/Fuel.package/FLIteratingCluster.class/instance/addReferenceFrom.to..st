analyzing
addReferenceFrom: anObject to: anotherObject
	| list |
	list := self references
		at: anObject
		ifAbsentPut: [ OrderedCollection new ].

	list add: anotherObject