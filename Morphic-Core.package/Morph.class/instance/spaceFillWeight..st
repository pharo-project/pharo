layout-properties
spaceFillWeight: aNumber
	"Layout specific. This property describes the relative weight that should be given to the receiver when extra space is distributed between different #spaceFill cells."
	aNumber = 1
		ifTrue:[self removeProperty: #spaceFillWeight]
		ifFalse:[self setProperty: #spaceFillWeight toValue: aNumber].
	self layoutChanged.