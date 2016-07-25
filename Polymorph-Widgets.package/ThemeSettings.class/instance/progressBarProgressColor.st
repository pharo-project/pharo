progressbar
progressBarProgressColor
	"Answer the value of progressBarProgressColor"

	^progressBarProgressColor ifNil: [self menuTitleColor]