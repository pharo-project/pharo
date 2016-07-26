initialization
initializeLayout
	"Initialize the layout."

	self
		changeTableLayout;
		layoutInset: 16;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		extent: self minimumExtent