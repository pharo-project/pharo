process list
updateProcessList
	| oldSelectedProcess newIndex now |
	now := Time millisecondClockValue.
	now - lastUpdate < 500
		ifTrue: [^ self].
	"Don't update too fast"
	lastUpdate := now.
	oldSelectedProcess := selectedProcess.
	processList := selectedProcess := selectedSelector := nil.
	Smalltalk garbageCollectMost.
	"lose defunct processes"

	processList := Process allSubInstances
				reject: [:each | each isTerminated].
	processList := processList
				sort: [:a :b | a priority >= b priority].
	processList := WeakArray withAll: processList.
	newIndex := processList
				indexOf: oldSelectedProcess
				ifAbsent: [0].
	self changed: #processNameList.
	self processListIndex: newIndex