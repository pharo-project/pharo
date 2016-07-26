building presentations
inspectorIn: aComposite

	^ aComposite dynamic 
		display: [:anObject |
			self inspectorIn: GLMCompositePresentation new on: anObject ]