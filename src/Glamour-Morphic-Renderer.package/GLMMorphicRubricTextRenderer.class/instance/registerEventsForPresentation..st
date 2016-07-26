private
registerEventsForPresentation: aPresentation
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMTextEditorRequest send: #actOnTextEditorRequest: to: self