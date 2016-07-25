accessing
add: aPresentation
	self pane notingPresentationChangeDo: [
		self presentations add: aPresentation.
		aPresentation pane: self pane ].
	^ aPresentation