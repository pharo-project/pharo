helpers
newCommand
	
	^ self commandClass new
			"For pipes, we can use a pipe for both reading or writing"
			defaultReadStreamCreationBlock: [self systemAccessor makeBlockingPipe];
			defaultWriteStreamCreationBlock: [self systemAccessor makeNonBlockingPipe];
			yourself 
	
	