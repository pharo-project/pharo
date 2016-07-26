layout
minHeight: aNumber 
	aNumber 
		ifNil: [self removeProperty: #minHeight]
		ifNotNil: [self setProperty: #minHeight toValue: aNumber].
	self layoutChanged