*Morphic-Widgets-Windows
navigateWindowBackward
	"Change the active window to the previous window."

	self previousWindow ifNotNil: [:m |
		m isCollapsed ifTrue: [m collapseOrExpand].
		m activate]