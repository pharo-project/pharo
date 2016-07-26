*Polymorph-Widgets
adoptPaneColor: paneColor
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self borderStyle baseColor: (self enabled ifTrue: [paneColor twiceDarker] ifFalse: [paneColor darker]).
	self fillStyle: self fillStyleToUse.
	self borderWidth > 0 
		ifTrue: [self borderStyle: self borderStyleToUse]