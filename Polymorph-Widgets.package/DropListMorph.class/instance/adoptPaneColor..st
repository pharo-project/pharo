accessing
adoptPaneColor: paneColor
	"Pass on to the list morph and border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: self fillStyleToUse.
	self borderWidth > 0 ifTrue: [
		self borderStyle: self borderStyleToUse].
	self buttonMorph cornerStyle: self cornerStyle.
	self updateContentColor: paneColor.
	self listPaneColor: paneColor.
	self changed: #buttonLabel