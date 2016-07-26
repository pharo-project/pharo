rendering
render: aPane
	paneMorph := GLMMorphic containerMorph.
	aPane when: GLMMatchingPresentationsChanged send: #actOnMatchingPresentationsChanged: to: self.
	self renderMorphFor: aPane.
	^ paneMorph