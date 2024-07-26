"
I am a repository representing a remove file bsed repository. I'll mostly add some caching of the code I'm loading so that if we load it mulitple times, we do not need to get the code from the remote location all the time.
"
Class {
	#name : 'MCRemoteFileBasedRepository',
	#superclass : 'MCFileBasedRepository',
	#category : 'MonticelloRemoteRepositories',
	#package : 'MonticelloRemoteRepositories'
}

{ #category : 'testing' }
MCRemoteFileBasedRepository class >> isAbstract [
	^ self = MCRemoteFileBasedRepository 
]

{ #category : 'testing' }
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

{ #category : 'fetching' }
MCRemoteFileBasedRepository >> fetchPackageNamed: aName [

	MCCacheRepository default storeVersion: (self packageNamed: aName)
]

{ #category : 'private' }
MCRemoteFileBasedRepository >> loadVersionFromFileNamed: aString [

	(MCCacheRepository uniqueInstance includesFileNamed: aString) ifTrue: [ ^ MCCacheRepository uniqueInstance loadVersionFromFileNamed: aString ].

	super loadVersionFromFileNamed: aString
]

{ #category : 'interface' }
MCRemoteFileBasedRepository >> versionWithInfo: aVersionInfo ifAbsent: errorBlock [
	"get a version for the given versionInfo. always query first the packageCache and only then try to load the version from the remote location"

	^ MCCacheRepository uniqueInstance versionWithInfo: aVersionInfo ifAbsent: [ super versionWithInfo: aVersionInfo ifAbsent: errorBlock ]
]
