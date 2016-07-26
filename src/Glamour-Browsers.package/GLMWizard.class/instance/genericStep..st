accessing - scripting
genericStep: aSymbol 

	self steps add: (aSymbol).
	^ self stepsDictionary at: aSymbol put: GLMWizardGenericStep new.