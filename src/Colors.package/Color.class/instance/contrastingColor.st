conversions
contrastingColor
	"Answer black or white depending on the luminance."

	self isTransparent ifTrue: [ ^ self class black].
	^self luminance > 0.5
		ifTrue: [self class black]
		ifFalse: [self class white]