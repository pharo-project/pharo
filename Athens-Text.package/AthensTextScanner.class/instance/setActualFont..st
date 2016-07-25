text attributes
setActualFont: aFont

	currentFont ifNil: [
		
		currentFont := AthensFontChange new font: aFont.
		self addCommand: currentFont.
		^ self 
	].

	aFont = currentFont font ifFalse: [
		currentFont := AthensFontChange new font: aFont.
		self addCommand: currentFont.
	].
