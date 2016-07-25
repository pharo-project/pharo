waiting
waitForExitPollingEvery: aDelay doing: aClosure
	" IMPORTANT: read first the comment of #waitForExit. 
	
	This method does NOT use #runAndWait nor #waitForExit. Instead, it does an image-based polling to check status of child process. 
	Note that the loop does send #queryExitStatus which is the method that indeeds does the waitpid() to check it.
	This is not strictly necessary because the childWatcher of the OSSVMProcess
	takes care of capturing SIGCHDL and update exitStatus. 
	However, as a more reliable solution, we can also check exit status here in case there are missed SIGCHLD or whatever.
	
	As part of the loop, besides checking the exit status (#queryExitStatus) it also evaluates aClosure
	
	"
	shouldContinueWaiting := true.
	[
		"If the #shutDown: (shouldContinueWaiting was set to false) happened while this process was in the #wait 
		 then we simply check 'shouldContinueWaiting' here before doing the #queryExitStatus"
		shouldContinueWaiting 
			ifTrue: [ self queryExitStatus ]
			ifFalse: [ ^ nil ].
		"nil answer means the child has not yet finished" 
		(exitStatus isNil and: [ shouldContinueWaiting ])
	]
	whileTrue: [ 
		aDelay wait.
		aClosure cull: self cull: stdoutStream cull: stderrStream.
	].
	"We might have more contents in the streams that could have been written since the last 'aClosure value' and the moment #queryExitStatus answers non nil.
	So just in case we execute aClosure one more time"
	shouldContinueWaiting 
		ifTrue: [ aClosure cull: self cull: stdoutStream cull: stderrStream. ].
	^ exitStatus  