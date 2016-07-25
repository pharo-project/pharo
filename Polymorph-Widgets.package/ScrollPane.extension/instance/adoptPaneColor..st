*Polymorph-Widgets
adoptPaneColor: paneColor
	"Match the pane colour."
	
	super adoptPaneColor: paneColor.
	scrollBar adoptPaneColor: paneColor.
	hScrollBar adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self borderWidth > 0 ifTrue: [
		self borderStyle: self borderStyleToUse]