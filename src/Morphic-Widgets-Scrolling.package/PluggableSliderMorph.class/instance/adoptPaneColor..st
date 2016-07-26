accessing
adoptPaneColor: paneColor
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self
		fillStyle: self fillStyleToUse;
		borderStyle: self borderStyleToUse;
		sliderColor: (self enabled
			ifTrue: [paneColor twiceDarker]
			ifFalse: [self paneColor twiceDarker paler])