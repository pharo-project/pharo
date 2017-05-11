building
compose
	self pager
		minimumPageNumberForScrollbar: 2; 
		show: [ :a :each | 
			a dynamic display: [ :x | 
				(GLMCompositePresentation new with: [ :aComposite | 
					each gtDebuggerPresentationsIn: aComposite inContext: self.
					self attachExtraBehaviourToPresentationsIn: aComposite.
					aComposite ])
						startOn: x ] ].
	self first when: GLMPaneAdded send: #actOnPaneAdded: to: self.
	self first when: GLMPaneRemoved send: #actOnPaneRemoved: to: self