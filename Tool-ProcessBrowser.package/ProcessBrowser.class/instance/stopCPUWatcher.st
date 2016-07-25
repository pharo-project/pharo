initialization
stopCPUWatcher

	CPUWatcher stopMonitoring.
	self updateProcessList.
	startedCPUWatcher := false	"so a manual restart won't be killed later" 