styles
spotterPreviewStyleFor: aBrick
	
	aBrick
		borderWidth: self previewBorderWidth;
		hSpaceFill: self previewWidthPercent;
		vSpaceFill;
		margin: { self headerHeight + self innerBorderWidth. 0 . 0};
		marginLeft: (self previewBorderWidth + self previewArrowWidth) negated.
	
	aBrick layout
		zIndex: 1