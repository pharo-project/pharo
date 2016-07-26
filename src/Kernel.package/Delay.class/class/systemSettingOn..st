settings
systemSettingOn: aBuilder
	<systemsettings>
	
	(aBuilder pickOne: #delaySchedulerClass)
		parent: #pharoSystem ;
		target: self;
		label: 'Delay Scheduler' ;
		domainValues: DelayScheduler allSubclasses ;
		description: 'Choose the class to use for Delay scheduling.' , 
			String crlf, 'You can observe which is running from Tools > Process Browser.'
	