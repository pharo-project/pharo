private
initializeAnnoucementForPresentation: aPresentation
	aPresentation when: GLMDataSourceUpdated send: #dataSourceUpdated: to: self.
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMPresentationUpdated send: #actOnUpdatedPresentation: to: self