*Morphic-Widgets-Windows
navigateVisibleWindowForward
	"Change the active window to the next visible and not collapsed window."
 
	self nextVisibleWindow
		ifNil: [SystemWindow passivateTopWindow]
		ifNotNil: [:m | m activate]
 