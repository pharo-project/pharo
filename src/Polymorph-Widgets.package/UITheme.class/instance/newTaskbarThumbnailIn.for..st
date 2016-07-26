morph creation
newTaskbarThumbnailIn: aThemedMorph for: aWindow
	"Answer a taskbar thumbnail morph for the given window."

	|answer thumb|
	thumb := aWindow taskbarThumbnail.
	answer := PanelMorph new
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		changeTableLayout;
		layoutInset: 8;
		cellInset: 4;
		addMorphBack: thumb;
		addMorphBack: (self
			buttonLabelForText: (aWindow labelString truncateWithElipsisTo: 50)).
	answer
		extent: answer minExtent;
		fillStyle: (self taskListFillStyleFor: answer);
		borderStyle: (self taskbarThumbnailNormalBorderStyleFor: aWindow);
		cornerStyle: (self taskbarThumbnailCornerStyleFor: answer).
	^answer