accessing
baseColor: aColor
	| cc |
	cc := aColor isTransparent ifTrue:[nil] ifFalse:[aColor].
	baseColor = cc ifTrue:[^self].
	baseColor := cc.
	self releaseCachedState.
	self color: cc.
