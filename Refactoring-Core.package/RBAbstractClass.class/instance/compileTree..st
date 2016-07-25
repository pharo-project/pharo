method accessing
compileTree: aRBMethodNode 
	^ (self methodFor: aRBMethodNode selector) compileTree: aRBMethodNode