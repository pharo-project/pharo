Morphic-Base-Windows
expandAll
	"Expand all windows"
	(self  windowsSatisfying: [:w | w isCollapsed])
		reverseDo: [:w | w collapseOrExpand.  self displayWorld]