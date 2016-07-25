text attributes
textBackground: aColor

	"background is transparent by default "
	
	(currentBackground notNil and: [ currentBackground color = aColor ])
		ifTrue: [ ^ self].
		
	currentBackground ifNil: [ aColor isTransparent ifTrue: [ ^ self ] ].
	
	currentBackground := AthensBackgroundChange new color: aColor.
	currentBackground start: currentLine width.
	currentLine addCommand: currentBackground.