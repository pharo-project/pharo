waiting
waitForExit
	"The waiting in this case is efficient since it is NOT done with a delay polling but using the SIGCHLD handler of OSSVMProcess childWatcher. The childWatcher when received a SIGCHLD will detect the child 
	that died and will send #processHasExitNotification to such a child. 
	In #processHasExitNotification the child will signal this 'mutexForSigchld' and hence
	make the #waitForExit to proceed.  
	
	IMPORTANT: This kind of waiting could generate a deadlock when the user uses pipes 
	for mapping standard streams and the child writes a lot of stuff into it. Basically, there is a problem in
	general with waiting for an external process to exit before reading its output. If the external process 
	creates a large amount of output, and if the output is a pipe, it will block on writing to the pipe until
	 someone (our VM process) reads some data from the pipe to make room for the writing. That leads to cases where 
	we never read the output (because the external process did not exit) and the external process never exits 
	(because we have not read the data from the pipe).
	
	
	Therefore, use this method (and all its senders) only when using FileStreams for the standard streams, 
	when not defining standard streams at all, or when you know the command to be executed does not write much in stdout/stderr."
	mutexForSigchld := Semaphore new.
	mutexForSigchld wait. 
	^ exitStatus  