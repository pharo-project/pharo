accessing
color: aColor
	color = aColor ifTrue:[^self].
	color := aColor.
	self releaseCachedState.