setting
cpuWatcherEnabled: aBoolean
	CpuWatcherEnabled = aBoolean ifTrue: [^ self].
	CpuWatcherEnabled := aBoolean.
	self monitorPreferenceChanged
