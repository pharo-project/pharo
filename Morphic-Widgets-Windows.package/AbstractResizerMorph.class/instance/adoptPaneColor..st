actions
adoptPaneColor: paneColor
	"Just get the resizer fill style for the theme."
	
	paneColor ifNil: [^super adoptPaneColor: paneColor].
	self fillStyle: (self theme resizerGripNormalFillStyleFor: self)