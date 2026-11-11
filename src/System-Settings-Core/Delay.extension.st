Extension { #name : 'Delay' }

{ #category : '*System-Settings-Core' }
Delay class >> systemSettingOn: aBuilder [

	<systemsettings>
	(aBuilder pickOne: #delaySchedulerClass)
		parentName: #pharoSystem;
		target: self;
		label: 'Delay Scheduler';
		domainValues: DelayNullScheduler allSubclasses;
		defaultValue: DelaySemaphoreScheduler;
		description:
			'Choose the class to use for Delay scheduling.' , String crlf
			, 'You can observe which is running from Tools > Process Browser.'
]
