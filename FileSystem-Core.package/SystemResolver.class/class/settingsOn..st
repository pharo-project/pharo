settings
settingsOn: aBuilder
	<systemsettings>  

	(aBuilder setting: #userLocalDirectory)
		parent: #pharoSystem;
		type: #Directory;
		target: self;
		description: 'The path of a directory where Pharo will store local files' ;
		label: 'Local directory'.