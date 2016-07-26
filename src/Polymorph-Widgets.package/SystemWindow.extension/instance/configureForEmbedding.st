*Polymorph-Widgets
configureForEmbedding
	"Set up the window so it can be embedded into another morph."

	labelArea owner notNil ifTrue: [
		self
			hasDropShadow: false;
			beWithoutGrips;
			removeLabelArea;
			makeBorderless.
		self submorphsDo: [:m | m unlock]]