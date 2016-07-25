rendering
render: aPresentation
	container := GLMMorphic containerMorph.
	container 
		addMorph: (self renderObject: aPresentation currentPresentation) 
		fullFrame: (LayoutFrame identity).
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	^ container