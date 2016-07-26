*Morphic-Widgets-Windows
openModal: aSystemWindow
	"Open the given window locking the receiver until it is dismissed.
	Set the pane color to match the current theme.
	Answer the system window."
	
	aSystemWindow setWindowColor: self theme windowColor.
	^ super openModal: aSystemWindow