preconditions
environmentWithReferencesTo: aClass
	^ RBBrowserEnvironment new 
		referencesTo: (Smalltalk globals 
			associationAt: aClass 
			ifAbsent: [ aClass ])