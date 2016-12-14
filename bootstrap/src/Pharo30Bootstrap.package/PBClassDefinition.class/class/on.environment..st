instance-creation
on: classDefinition environment: aBootstrapEnvironment 
	
	^ self new
		initializeOn: classDefinition environment: aBootstrapEnvironment;
		yourself