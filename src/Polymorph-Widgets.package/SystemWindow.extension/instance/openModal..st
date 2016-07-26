*Polymorph-Widgets
openModal: aSystemWindow
	"Open the given window locking the receiver until it is dismissed.
	Set the pane color to match the receiver.
	Answer the system window."
	
	aSystemWindow
		theme: self theme;
		setWindowColor: self paneColor.
	^super openModal: aSystemWindow