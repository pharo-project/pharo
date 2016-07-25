accessing colors
adoptPaneColor: paneColor
	"Pass on to the selection, the border and on the eventual pager too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self pager ifNotNil: [:p | p color: self pagerColor]