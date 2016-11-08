comparing
= aColor
	"Return true if the receiver equals the given color. This method handles translucent colors, too."

	aColor isColor ifFalse: [^ false].
	^ aColor privateRGB = rgb and:
		[aColor privateAlpha = self privateAlpha]
