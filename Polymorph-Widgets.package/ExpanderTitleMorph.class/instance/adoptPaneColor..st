accessing
adoptPaneColor: paneColor
	"Update the fill styles, corner styles, label colour
	and expansion button indicator."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: self normalFillStyle.
	self borderStyle baseColor: paneColor twiceDarker.
	self buttonMorph cornerStyle: self cornerStyle.
	self labelMorph color: paneColor contrastingColor.
	self changed: #expandLabel