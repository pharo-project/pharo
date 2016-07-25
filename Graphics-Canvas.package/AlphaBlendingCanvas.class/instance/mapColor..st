private
mapColor: aColor
	aColor isColor ifFalse:[^aColor]. "Should not happen but who knows..."
	aColor isTransparent ifTrue:[^aColor].
	aColor isOpaque ifTrue:[^aColor alpha: alpha].
	^aColor alpha: (aColor alpha * alpha)