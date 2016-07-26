rendering
registerEventsForPresentation: aPresentation
	super registerEventsForPresentation: aPresentation.
	aPresentation when: GLMVariableBindingsChanged send: #actOnVariableBindingsUpdated: to: self.
	aPresentation when: GLMEvaluateSelection send: #actOnEvaluateSelection: to: self.
	aPresentation when: GLMCompileText send: #actOnCompileText: to: self.
	aPresentation when: GLMDebugSelection send: #debugSelection to: self.
	aPresentation when: GLMProfileSelection send: #profileSelection to: self.
	aPresentation when: GLMPrintSelection send: #evaluateAndPrintSelection to: self