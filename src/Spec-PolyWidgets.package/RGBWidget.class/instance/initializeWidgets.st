initialization
initializeWidgets
	preview := self instantiate: ImageModel.
	sliders := self instantiate: RGBSliders.
	preview image: (Form extent: 30 @ 30 depth: 32).
	preview image: self makeNewForm.
	self focusOrder add: sliders