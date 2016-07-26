accessing - scripting
step: aSymbol 

	self steps add: (aSymbol).
	^ self stepsDictionary at: aSymbol put: GLMWizardStep new.