rendering
render: aMorphPresentation
	aMorphPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	^ (scrollPane := self scrollMorphFor: (self morphFrom: aMorphPresentation))