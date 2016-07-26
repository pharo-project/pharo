process control
unregisterWellKnownProcess: aProcess
	"Remove the first registry entry that matches aProcess. Use
	with caution if more than one registry entry may match aProcess."

	"self unregisterWellKnownProcess: Smalltalk lowSpaceWatcherProcess"

	WellKnownProcesses detect: [ :e | e key value == aProcess ] ifFound: [ :entry | WellKnownProcesses remove: entry ]