accessing
drawOn: aCanvas
	drawable
		ifFalse: [container buildRowMorphsFrom: self]
		ifTrue: [super drawOn: aCanvas]
