drawing
drawOn: aCanvas
	(owner notNil and: [owner containsPoint: self position])
		ifTrue: [super drawOn: aCanvas]
