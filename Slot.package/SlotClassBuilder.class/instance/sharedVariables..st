accessing
sharedVariables: anArray 
	sharedVariables := anArray collect: [ :each | each asClassVariable ]