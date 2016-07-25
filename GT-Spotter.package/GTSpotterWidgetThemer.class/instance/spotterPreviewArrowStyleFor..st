styles
spotterPreviewArrowStyleFor: aBrick

	aBrick
		hSpaceFill;
		renderer: (GTSpotterTriangleRenderer new orientation: #right);
		height: [ (self previewArrowWidth - self previewArrowGapHack ) * 2 ]