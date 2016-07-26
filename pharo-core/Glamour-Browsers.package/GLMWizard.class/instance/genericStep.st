accessing - scripting
genericStep

	|tmpSymbol|
	tmpSymbol := (self defaultStepName, self steps size asString) asSymbol.
	self steps add: tmpSymbol.
	^ self stepsDictionary at: tmpSymbol put: GLMWizardGenericStep new.