*Morphic-Widgets-Windows
navigateWindowForward
	"Change the active window to the next window."

	self nextWindow ifNotNil: [:m |
		self currentWindow ifNotNil: [:w | w sendToBack].
		m isCollapsed ifTrue: [m collapseOrExpand].
		m activate]