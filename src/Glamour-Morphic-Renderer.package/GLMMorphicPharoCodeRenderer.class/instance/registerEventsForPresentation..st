private
registerEventsForPresentation: aPresentation
	super registerEventsForPresentation: aPresentation.
	aPresentation when: GLMSelectCurrentLine send: #selectLine to: self.
	aPresentation when: GLMEvaluateSelection send: #actOnEvaluateSelection: to: self.
	aPresentation when: GLMHighlightAndEvaluate send: #actOnHighlightAndEvaluate: to: self.
	aPresentation when: GLMCompileText send: #actOnCompileText: to: self.
	aPresentation when: GLMDebugSelection send: #debugHighlightedText to: self.
	aPresentation when: GLMProfileSelection send: #profileHighlightedText to: self.
	aPresentation when: GLMPrintSelection send: #evaluateAndPopPrintHighlight to: self