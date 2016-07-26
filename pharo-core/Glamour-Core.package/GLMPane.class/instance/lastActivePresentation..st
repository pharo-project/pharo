accessing
lastActivePresentation: aPresentation
	(self port: #activePresentation) value: aPresentation.
	lastActivePresentation := aPresentation.
	self browser lastActivePresentation: aPresentation