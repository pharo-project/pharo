actions
executeAction

	self needsUpdate: false.
	self codePresentation hasUnacceptedEdits 
		ifTrue: [
			self codePresentation 
				compileTextIn: self currentContext 
				from: self session 
				andDo: [ self needsUpdate: true ] ].
	self codePresentation flash