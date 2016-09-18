settings
defaultTimeLimitSettingOn: aBuilder
	<systemsettings>
	
	(aBuilder setting: #defaultTimeLimitSecs)
		target: self;
		parent: #pharoSystem ;
		label: 'Default time limit for tests (secs)' ;
		description: 'Detault time limit in seconds for test execution'