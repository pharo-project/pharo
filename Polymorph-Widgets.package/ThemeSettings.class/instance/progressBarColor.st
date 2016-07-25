progressbar
progressBarColor
	"Answer the value of progressBarColor"

	^progressBarColor ifNil: [progressBarColor := self derivedMenuColor muchLighter]