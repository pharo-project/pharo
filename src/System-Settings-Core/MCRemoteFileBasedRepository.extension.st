Extension { #name : 'MCRemoteFileBasedRepository' }

{ #category : '*System-Settings-Core' }
MCRemoteFileBasedRepository class >> monticelloSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder setting: #cacheDirectory)
		type: #Directory;
		parent: #monticello;
		target: MCCacheRepository;
		default: (FileLocator localDirectory / 'package-cache') asFileReference;
		description: 'The path of the local repository cache';
		label: 'Local cache directory'
]
