building pane
computePaneWith: aValidator
	| tmpPane |
	tmpPane := GLMPane in: self.
	tmpPane addPresentation: aValidator.
	(tmpPane port: #entity) value: self stepToUse input.
	self updateBrowserTitle.
	^ tmpPane