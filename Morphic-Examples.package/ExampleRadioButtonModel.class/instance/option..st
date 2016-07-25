accessing
option: aSymbol
	"Set the value of option"

	option := aSymbol.
	self
		changed: #isLeft;
		changed: #isCenter;
		changed: #isRight