*Polymorph-Widgets
modalLockTo: aSystemWindow
	"Lock the receiver as a modal owner of the given window."

	aSystemWindow
		setProperty: #modalOwner toValue: self.
	self setProperty: #modalChild toValue: aSystemWindow.
	
	closeBox ifNotNil: [ 
		self setProperty: #preModalCloseEnabled toValue: closeBox enabled.
		closeBox enabled: false
	].
