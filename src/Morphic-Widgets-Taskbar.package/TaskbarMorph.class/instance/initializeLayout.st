initialization
initializeLayout
	"Initialize the layout."

	self
		changeTableLayout;
		layoutInset: 2;
		cellInset: 2;
		listDirection: #leftToRight;
		wrapDirection: #topToBottom;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		extent: self minimumExtent