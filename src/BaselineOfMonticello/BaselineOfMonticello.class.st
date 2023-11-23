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
			package: 'Zinc-Resource-Meta-Core';
			package: 'System-Changes';
			package: 'Compression';
			package: 'Monticello-Model';
			package: 'Monticello';
			package: 'Ring-Definitions-Monticello';
			package: 'System-Model';
			
			package: 'Network-Kernel';
			package: 'Network-MIME';
			package: 'Network-Protocols';
			package: 'MonticelloRemoteRepositories';
			package: 'Zinc-HTTP';
			package: 'Zodiac-Core'.
		spec 
			group: 'Core' with: #('Ring-OldChunkImporter' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Ring-Definitions-Core' 'Compression' 'Monticello-Model' 'Monticello' 'Ring-Definitions-Monticello' 'System-Model');
			group: 'RemoteRepositories' with: #( 'Network-Kernel' 'Network-MIME' 'Network-Protocols' 'MonticelloRemoteRepositories' 'Zinc-HTTP' 'Zodiac-Core' );

			group: 'default' with: #('Core' 'RemoteRepositories' ). ].
]
