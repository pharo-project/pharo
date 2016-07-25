private
registerEventsForPresentation: aPresentation

	super registerEventsForPresentation: aPresentation.
	
	aPresentation when: GLMVariableBindingsChanged send: #actOnVariableBindingsUpdated: to: self.
	aPresentation when: GLMVariableBindingsAccessRequest send: #actOnVariableBindingsAccessRequest: to: self.
	