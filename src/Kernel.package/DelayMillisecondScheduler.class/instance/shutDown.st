snapshotting
shutDown
	"Suspend the active delay, if any, before snapshotting. It will be reactived when the snapshot is resumed."
	"Details: This prevents a timer interrupt from waking up the active delay in the midst snapshoting, since the active delay will be restarted when resuming the snapshot and we don't want to process the delay twice."
	accessProtect consumeAllSignals.
	self primSignal: nil atMilliseconds: 0.
	self saveResumptionTimes.
	delaySuspended := true.
