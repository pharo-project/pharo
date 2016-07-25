Morphic-Base-Windows
collapseAll
	"Collapse all windows"
	(self windowsSatisfying: [:w | w isCollapsed not])
		reverseDo: [:w | w collapseOrExpand.  self displayWorld].
	self collapseNonWindows