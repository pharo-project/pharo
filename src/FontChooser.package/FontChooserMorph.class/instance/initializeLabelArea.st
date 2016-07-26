initialize-release
initializeLabelArea
	"Customize the window bar by removing all the decorations"
	
	super initializeLabelArea.
	self
		removeCollapseBox;
		removeExpandBox;
		removeMenuBox