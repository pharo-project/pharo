accessing - scripting
step


	|tmpSymbol|
	tmpSymbol := (self defaultStepName, self steps size asString) asSymbol.
	self steps add: tmpSymbol.
	^ self stepsDictionary at: tmpSymbol put: GLMWizardStep new.