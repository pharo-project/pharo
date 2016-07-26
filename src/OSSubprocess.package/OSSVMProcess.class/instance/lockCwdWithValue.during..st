cwd
lockCwdWithValue: cwdNewValue during: aBlock
	"This method is a complete hack in order to support a #cmd: option in OSSUnixSubprocess. 
	OSSUnixSubprocess relies in posix_spawn() family of functions to spwan processes in the OS,
	and these functions do not allow specifying a starting working directory for the child process. 
	However, this is sometimes needed by subprocess.
	Therefore, we propose this hack. Basically, we change the current directory of the parent process (vmProcess) 
	BEFORE launching the subprocess. When the child is launched, it inherits the working directory of the parent.
	As soon as the child was launched, we rollback and put back the original working directory in the parent. 
	Because if this, it's very much likely that aBlock represents the launching of a child process.

	mutexForCwd is needed because we may have multiple children at the same time trying to change working
	directory (not thread safe..). So with mutexForCwd we sincronize this. 	

	Forking with userInterruptPriority is useful just in case there could be other Smalltalk process 
	using another code, trying to ALSO change the working directory (very very unlikely). So at least with 
	userInterruptPriority we make sure that we a higher priority than the rest of the normal user code
	and so we minimize that problem.
	
	"

	| completed |
	cwdNewValue asFileReference exists ifFalse: [ ^ self error: 'The directory: ', cwdNewValue, ' does not exist' ]. 
	completed :=  Semaphore new. 
	[ 
		| oldCwd |
		oldCwd := self systemAccessor getcwd.
		[
			self systemAccessor chdir: cwdNewValue.
			mutexForCwd critical: aBlock.
		]
		ensure: [ 
			self systemAccessor chdir: oldCwd.
			completed signal.	
		]
	] forkAt: Processor userInterruptPriority.
	completed wait.
	