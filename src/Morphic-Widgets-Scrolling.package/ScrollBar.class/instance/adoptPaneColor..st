access
adoptPaneColor: aColor
	"Adopt the given pane color"
	aColor ifNil:[^self].
	self sliderColor: aColor.