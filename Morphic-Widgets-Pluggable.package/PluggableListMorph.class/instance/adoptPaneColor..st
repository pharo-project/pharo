drawing
adoptPaneColor: paneColor
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self selectionColor: self selectionColor.
	self fillStyle: self fillStyleToUse.
	self borderWidth > 0 ifTrue: [
		self borderStyle: self borderStyleToUse]