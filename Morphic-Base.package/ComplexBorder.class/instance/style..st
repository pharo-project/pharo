accessing
style: newStyle
	style == newStyle ifTrue:[^self].
	style := newStyle.
	self releaseCachedState.