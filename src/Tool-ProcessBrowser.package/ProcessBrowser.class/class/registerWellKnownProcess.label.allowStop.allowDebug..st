process control
registerWellKnownProcess: aBlockForProcess label: nickname allowStop: allowStop allowDebug: allowDebug
	"Add an entry to the registry of well known processes. aBlockForProcess
	evaluates to a known process to be identified by nickname, and allowStop
	and allowDebug are flags controlling allowable actions for this process
	in the browser."

	WellKnownProcesses add: aBlockForProcess->[{nickname . allowStop . allowDebug}]