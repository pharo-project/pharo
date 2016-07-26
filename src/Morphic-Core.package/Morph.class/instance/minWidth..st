layout
minWidth: aNumber 
	aNumber 
		ifNil: [self removeProperty: #minWidth]
		ifNotNil: [self setProperty: #minWidth toValue: aNumber].
	self layoutChanged