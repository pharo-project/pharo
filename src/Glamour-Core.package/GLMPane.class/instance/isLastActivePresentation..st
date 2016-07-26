testing
isLastActivePresentation: aPresentation
	^ self lastActivePresentation notNil and: [ 
		aPresentation title = self lastActivePresentation title ]