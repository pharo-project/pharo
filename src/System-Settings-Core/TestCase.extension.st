Extension { #name : 'TestCase' }

{ #category : '*System-Settings-Core' }
TestCase class >> defaultTimeLimitSettingOn: aBuilder [

	<systemsettings>
	(aBuilder group: #sunit)
		label: 'SUnit';
		parentName: #pharoSystem;
		description: 'SUnit';
		with: [
				(aBuilder setting: #defaultTimeLimitSecs)
					target: self;
					defaultValue: 10.0;
					label: 'Default time limit for tests (secs)';
					description: 'Detault time limit in seconds for test execution' ]
]
