monitoring
startMonitoringPeriod: pd rate: rt threshold: th
	"CPUWatcher startMonitoring"

	CurrentCPUWatcher ifNotNil: [ ^CurrentCPUWatcher startMonitoring. ].
	CurrentCPUWatcher := (self new)
		monitorProcessPeriod: pd sampleRate: rt;
		threshold: th;
		yourself.
	^CurrentCPUWatcher
