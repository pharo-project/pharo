accessing
formatterClass
	^ FormatterClass ifNil: [ RBNullFormatter ] ifNotNil: [ FormatterClass ]