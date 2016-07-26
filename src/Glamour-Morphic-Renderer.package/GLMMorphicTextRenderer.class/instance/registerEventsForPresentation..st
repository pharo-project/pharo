rendering
registerEventsForPresentation: aPresentation
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMSelectCurrentLine send: #selectLine to: self