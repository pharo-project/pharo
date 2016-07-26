waiting
stopWaiting
	"This will work only for the High Methods API that rely in the waiting provided by the framework. If the user
	user custom waiting, this won't work.
	The wait inside this framework could be done either from #waitForExit with mutexForSigchld or via  #waitForExitPollingEvery:doing:
	with the delay polling"
	mutexForSigchld ifNotNil: [ mutexForSigchld signal ].
	(shouldContinueWaiting = true) ifTrue: [ shouldContinueWaiting := false. ] 
	