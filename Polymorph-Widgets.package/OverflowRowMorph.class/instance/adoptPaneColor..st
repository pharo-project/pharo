accessing
adoptPaneColor: paneColor
	"Pass on to the list morph and border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self moreButton cornerStyle: self cornerStyle.
	self changed: #moreButtonLabel.
	self moreMorphs do: [:m |
		m adoptPaneColor: paneColor]