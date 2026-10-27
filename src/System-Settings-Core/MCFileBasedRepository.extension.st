Extension { #name : 'MCFileBasedRepository' }

{ #category : '*System-Settings-Core' }
MCFileBasedRepository class >> monticelloSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #monticello)
		label: 'Monticello';
		parentName: #SCM;
		description: 'All Monticello settings';
		with: [
				(aBuilder setting: #defaultDirectoryName)
					type: #Directory;
					target: MCDirectoryRepository;
					description: 'The path of a directory where you want to start out when choosing local repository locations';
					defaultValue: FileSystem workingDirectory fullName;
					label: 'Default local repository directory' ]
]
