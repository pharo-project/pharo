Extension { #name : 'TestCase' }

{ #category : '*System-Settings-Core' }
TestCase class >> defaultTimeLimitSettingOn: aBuilder [
	<systemsettings>

	(aBuilder group: #sunit)
		label: 'SUnit';
		parent: #pharoSystem;
		description: 'SUnit';
		with: [
			(aBuilder setting: #defaultTimeLimitSecs)
				target: self;
				default: 10.0;
				label: 'Default time limit for tests (secs)' ;
				description: 'Detault time limit in seconds for test execution']
]
