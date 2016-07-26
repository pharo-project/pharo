accessing
presentations: aCompositePresentation
	self notingPresentationChangeDo: [
		presentations pane: nil.
		presentations resetAnnouncer.
		aCompositePresentation pane: self.
		presentations := aCompositePresentation.
		presentations initializePresentation ]