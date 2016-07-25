private
mapColor: aColor
	aColor isColor ifFalse:[^aColor]. "Should not happen but who knows..."
	^aColor isTransparent
		ifTrue:[aColor]
		ifFalse:[shadowColor]