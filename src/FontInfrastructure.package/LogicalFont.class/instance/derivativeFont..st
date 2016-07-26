derivatives
derivativeFont: newFont
	"add aFont as derivative, answer new basefont"
	(self isRegular and: [newFont isRegular not]) ifTrue: [
		self derivativeFontsAt: newFont emphasis put: newFont.
		^self].
	"new font is base, copy everything over"
	self isRegular
		ifFalse: [newFont derivativeFontsAt: self emphasis put: self].
	self derivativeFonts do: [:f |
		newFont derivativeFontsAt: f emphasis put: f].
	derivatives := nil.
	^newFont