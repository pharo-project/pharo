child watching
initializeChildWatcher
	"This is a process which waits for the death of a child processes. Use SIGCHLD events rather than a Delay to poll."

	| processSynchronizationDelay |
	processSynchronizationDelay := Delay forMilliseconds: 20.
	childWatcher	 ifNil: [
		childWatcher := [[
				"OSProcess authors suspected that there were various ways in which OS signals
				could be missed under conditions of heavy load. For that reason, we use
				#waitTimeoutMSecs: with the semaphore, so that if a signal is missed,
				we time out and rescan occasionally anyway (#updateActiveChildrenAndNotifyDead
				sends queryExitStatus which sends waitpid() )
				"
				self sigChldSemaphore waitTimeoutMSecs: 1000.
				processSynchronizationDelay wait. "Avoids lost signals in heavy process switching"
				self updateActiveChildrenAndNotifyDead.
				] repeat] newProcess.

		childWatcher resume.
		"name selected to look reasonable in the process browser"
		childWatcher name: ((ReadStream on: childWatcher hash asString) next: 5)
				, ': the OSSubprocess child watcher'
	]
