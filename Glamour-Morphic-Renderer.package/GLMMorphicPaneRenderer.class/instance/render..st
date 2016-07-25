rendering
render: aPane
	container := GLMMorphic containerMorph.
	self renderContainerFor: aPane.
	aPane when: GLMMatchingPresentationsChanged send: #actOnMatchingPresentationChanged: to: self.
	(aPane presentations hasTitle or: [aPane presentations hasTitleIcon]) ifTrue: [
		aPane when: GLMPresentationToolbarUpdated send: #actOnPresentationToolbarUpdated: to: self. ].
	^ container