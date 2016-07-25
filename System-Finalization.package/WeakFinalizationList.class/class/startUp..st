system startup
startUp: resuming 
	resuming ifTrue: [ 
		HasNewFinalization := Smalltalk vm isPharoVM ]