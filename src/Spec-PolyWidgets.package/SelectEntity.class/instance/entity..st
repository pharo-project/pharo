api
entity: anObject
	entity value: anObject.
	anObject ifNotNil: [  
		self entityText text: (anObject perform: self displaySymbol)] ifNil: [ self entityText text: '' ]