OSSUnixSubprocess is the main interface for spwaning subprocesses in a Unix OS. 

A process consist at minimun of a program/command to be executed (a Unix binary..could be cat , ls, cp,  custom shell scripts, etc etc.  ) and an optional list of arguments that will be passed to that program.

This class also allows you to define streams  (either StandardFileStream or OSSPipe) that will be mapped to the underlying standard streams stdout/stderr/stdin. This way it provides an API for reading and writing from those. In addition, it takes care of opening, closing, cleaning and all the rest of streams operations. 

It also provides code for checking the status or waiting the exit of the OS process. Different strategies are supported (SIGCHLD based or delay polling). See the different methods in the protocol 'running'.' In addition, once the subprocess was started, the subprocess instance is registered in OSSVMProcess which takes care of handling the child death via the childWatcher. 

Some additional features involved env variable settings (environmentAt:put:) for the child, defining a working directory (#workingDirectory: ), facilities for shell commands, etc.

To achieve it's goals, this class relies on OSSUnixSystemAccessor for accessing Unix system calls. 

As for  implementation details to spwan processes this class relies on the posix_spwan() family of functions which we call via FFI. 

The following is ONE example of ONE possible usage. We define and set settings which are actually the default, so they wouldn't make sense..but just for showing the most of the API:

OSSUnixSubprocess new	
	command: 'ls';
	arguments: (Array with: Smalltalk image imagePath);
	defaultWriteStreamCreationBlock: [ OSSVMProcess vmProcess systemAccessor makeNonBlockingPipe ];
	redirectStdout; "automatic default stream creation...above closure."
	redirectStderrTo: '/tmp/stderrFile.txt' asFileReference writeStream; "custom stream creation"
	createMissingStandardStreams: false; "therefore won't create stdin stream"
	workingDirectory: '/home'; "set working directory for child"
	environmentAt: 'HOME' put: '/tmp/home';
	addAllEnvVariablesFromParentWithoutOverride; "we will inherit then all but $HOME"
	runAndWaitOnExitDo: [ :command :outString :errString |
		self assert: (outString includesSubstring: Smalltalk image imagePath).	
		self assert: command isSuccess.
		self assert: errString isEmpty. 
	]
	 



