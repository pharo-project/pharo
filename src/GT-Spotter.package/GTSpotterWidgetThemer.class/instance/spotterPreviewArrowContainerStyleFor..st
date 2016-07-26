styles
spotterPreviewArrowContainerStyleFor: aBrick

	aBrick
		useVerticalLinearLayout;
		vSpaceFill;
		width: self previewArrowWidth;
		margin: {
			self headerHeight + self innerBorderWidth + self previewBorderWidth.
			0.
			self previewBorderWidth.
			self previewArrowGapHack negated }.
		
	aBrick layout
		zIndex: 4.