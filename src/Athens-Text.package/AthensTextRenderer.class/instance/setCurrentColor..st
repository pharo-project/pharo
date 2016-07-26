as yet unclassified
setCurrentColor: aColor
	color = aColor ifTrue: [ ^ self ].
	color := aColor.
"	canvas setPaint: aColor."
	glyphRenderer ifNotNil:  [ glyphRenderer setColor: aColor ].