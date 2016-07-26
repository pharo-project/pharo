instance-creation
onStartup: aBlockClosure 
	
	^ self new
		startupHandler: aBlockClosure;
		yourself