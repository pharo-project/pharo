"
This baseline is used to store information about Monticello packages. It is not directly used for the packages loading during bootstrap.
"
Class {
	#name : #BaselineOfMonticello,
	#superclass : #BaselineOf,
	#category : #BaselineOfMonticello
}

{ #category : #accessing }
BaselineOfMonticello class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : #accessing }
BaselineOfMonticello class >> remoteRepositoriesPackageNames [

	^ self packagesOfGroupNamed: #RemoteRepositories
]

{ #category : #baseline }
BaselineOfMonticello >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec 
			package: 'Ring-Definitions-Core';
			package: 'Ring-Definitions-Containers';
			package: 'Ring-OldChunkImporter';
			package: 'Zinc-Resource-Meta-Core';
			package: 'System-Changes';
			package: 'Compression';
			package: 'Monticello';
			package: 'Ring-Definitions-Monticello';
			package: 'System-Model';
			
			package: 'Network-Kernel';
			package: 'Network-MIME';
			package: 'Network-Protocols';
			package: 'MonticelloRemoteRepositories';
			package: 'Zinc-HTTP';
			package: 'Zinc-FileSystem';
			package: 'Zodiac-Core'.
		spec 
			group: 'Core' with: #('Ring-OldChunkImporter' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Ring-Definitions-Core' 'Ring-Definitions-Containers' 'Compression' 'Monticello' 'Ring-Definitions-Monticello' 'System-Model');
			group: 'RemoteRepositories' with: #( 'Network-Kernel' 'Network-MIME' 'Network-Protocols' 'MonticelloRemoteRepositories' 'Zinc-HTTP' 'Zinc-FileSystem' 'Zodiac-Core' );

			group: 'default' with: #('Core' 'RemoteRepositories' ). ].		

]
