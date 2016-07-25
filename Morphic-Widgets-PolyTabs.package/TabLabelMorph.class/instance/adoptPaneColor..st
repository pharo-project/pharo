accessing
adoptPaneColor: paneColor
	"Use the theme for fillStyle and border."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: self fillStyleToUse.
	self borderStyle: self borderStyleToUse