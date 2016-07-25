accessing
actualProcesses
	^ Process allSubInstances reject: [ :each | each isTerminated ]