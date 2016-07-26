startup-shutdown
startMonitoring
	"CPUWatcher startMonitoring"

	^self startMonitoringPeriod: 20 rate: 100 threshold: 0.8