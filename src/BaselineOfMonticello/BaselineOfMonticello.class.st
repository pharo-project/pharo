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
			package: 'System-Changes';
			package: 'Jobs';
			package: 'Compression';
			package: 'Monticello-Model';
			package: 'Monticello';
			package: 'Ring-Definitions-Monticello';
			
			package: 'Network-Kernel';
			package: 'Network-Protocols';
			package: 'Zinc-Resource-Meta-Core';
			package: 'Network-MIME';
			package: 'Zinc-HTTP';
			package: 'MonticelloRemoteRepositories';
			package: 'Zodiac-Core'.
		spec 
			group: 'Core' with: #('System-Changes' 'Ring-Definitions-Core' 'Ring-OldChunkImporter' 'Jobs' 'Compression' 'Monticello-Model' 'Monticello' 'Ring-Definitions-Monticello');
			group: 'RemoteRepositories' with: #( 'Network-Kernel' 'Network-Protocols' 'Zinc-Resource-Meta-Core' 'Network-MIME' 'Zinc-HTTP' 'MonticelloRemoteRepositories' 'Zodiac-Core' );

			group: 'default' with: #('Core' 'RemoteRepositories' ). ].
]
