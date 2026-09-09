Extension { #name : 'ProcessorScheduler' }

{ #category : '*System-Settings-Core' }
ProcessorScheduler class >> idleTimeSettingOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #idleTime)
		target: self;
		parent: #pharoSystem ;
		default: 50000;
		label: 'Default idle time for the system (microseconds)' ;
		description: 'Detault idle time for the Pharo process in case of inactivity. In idle mode, the system can be awakened by a signal. This value can limit the minimal delay time.'
]
