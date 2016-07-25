initialize - release
initialize
	super initialize.
	envVariables := Dictionary new. 
	createMissingStandardStreams := false.
	"By default, we create a none blocking pipe for writers (stdout and stderr). For reader, we prefer blocking pipes (stdin), otherwise the child will exit quickly. We also allow regular StandardFileStreams instances."
	defaultReadStreamCreationBlock := [self systemAccessor makeBlockingPipe].
	defaultWriteStreamCreationBlock := [self systemAccessor makeNonBlockingPipe].
	retrievedStdout := String new writeStream. 
	retrievedStderr := String new writeStream. 
	terminateOnShutdown := false.
	
	
		
