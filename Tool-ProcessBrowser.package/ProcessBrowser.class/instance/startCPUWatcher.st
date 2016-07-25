initialization
startCPUWatcher
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring
		ifFalse: [ 
					CPUWatcher startMonitoringPeriod: 5 rate: 100 threshold: 0.85.
					self setUpdateCallbackAfter: 7.
					^ true ]