text attributes
textColor: aColor

	currentColor ifNil: [
		currentColor := AthensColorChange new color: aColor.
		self addCommand: currentColor.
		^ self 
	].

	currentColor color = aColor ifFalse: [
		currentColor := AthensColorChange new color: aColor.
		self addCommand: currentColor.	
	].
	