actions
compileTextIn: aContext from:  aSession andDo: aBlock

	self announce: (GLMCompileText new
		presentation: self;
		context: aContext;
		session: aSession;
		action: aBlock)