accessing
knownProcesses
	"Return a dynamic list of know processes"
	
	"(self actualProcesses difference: self knownProcesses) do: [ :each | each suspend; terminate ]."
	
	^ {
		Processor activeProcess.	
		InputEventFetcher default fetcherProcess.
		WeakArray runningFinalizationProcess.
		Processor backgroundProcess.
		SmalltalkImage current lowSpaceWatcherProcess.
		UIManager default uiProcess.
		Delay schedulingProcess } asSet