Extension { #name : 'SystemResolver' }

{ #category : '*System-Settings-Core' }
SystemResolver class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #userLocalDirectory)
		parent: #pharoSystem;
		type: #Directory;
		target: self;
		default: self defaultLocalDirectory;
		description: 'The path of a directory where Pharo will store local files' ;
		label: 'Local directory'
]
