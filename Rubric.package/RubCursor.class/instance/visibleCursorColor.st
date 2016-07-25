accessing
visibleCursorColor
	self class cursorWithFixedColor 
		ifTrue: [ ^ self class defaultColor ].
	^ visibleCursorColor ifNil: [ self defaultVisibleCursorColor ] 