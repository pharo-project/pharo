*Polymorph-Widgets
configureForUnembedding
	"Set up the window so it can be unembedded and placed in the world."

	labelArea owner isNil ifTrue: [
		self
			makeBordered;
			initializeLabelArea;
			themeChanged;
			addGrips]