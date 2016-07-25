drop shadows
shadowColor: aColor
	self shadowColor = aColor ifFalse: [self changed].
	self setProperty: #shadowColor toValue: aColor.