"
This baseline is used to store information about Monticello packages. It is not directly used for the packages loading during bootstrap.
"
Class {
	#name : 'BaselineOfMonticello',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfMonticello',
	#package : 'BaselineOfMonticello'
}

{ #category : 'accessing' }
BaselineOfMonticello class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'accessing' }
BaselineOfMonticello class >> remoteRepositoriesPackageNames [

	^ self packagesOfGroupNamed: #RemoteRepositories
]

{ #category : 'baselines' }
BaselineOfMonticello >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec 
			package: 'Ring-Definitions-Core';
			package: 'Ring-OldChunkImporter' with: [ spec requires: #('Ring-Definitions-Core') ];
			package: 'Jobs';
			package: 'Random-Core';
			package: 'System-Hashing';
			package: 'Compression';
			package: 'Monticello-Model';
			package: 'Monticello';
			package: 'Ring-Definitions-Monticello';
			
			package: 'Network-Kernel';
			package: 'Network-Protocols';
			package: 'Network-MIME';
			package: 'Zinc-HTTP';
			package: 'MonticelloRemoteRepositories';
			package: 'Zodiac-Core';
			
			package: 'Compression-Tests';
			package: 'Jobs-Tests';
			package: 'Random-Tests';
			package: 'Ring-Definitions-Core-Tests';
			package: 'Monticello-Tests';
			package: 'MonticelloMocks';
			package: 'Ring-Definitions-Monticello-Tests';
			package: 'Ring-Definitions-Tests-Containers'.

		spec 
			group: 'Core' with: #('Ring-Definitions-Core' 'Ring-OldChunkImporter' 'Jobs' 'Random-Core' 'System-Hashing' 'Compression' 'Monticello-Model' 'Monticello' 'Ring-Definitions-Monticello');
			group: 'RemoteRepositories' with: #( 'Network-Kernel' 'Network-Protocols' 'Network-MIME' 'Zinc-HTTP' 'MonticelloRemoteRepositories' 'Zodiac-Core' );
			group: 'Tests' with: #( 'Compression-Tests' 'Jobs-Tests' 'Random-Tests' 'Ring-Definitions-Core-Tests' 'Monticello-Tests' 'MonticelloMocks' 'Ring-Definitions-Monticello-Tests' 'Ring-Definitions-Tests-Containers') ].
]
