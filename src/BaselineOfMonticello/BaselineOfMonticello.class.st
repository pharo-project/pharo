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
			package: 'Ring-Deprecated-Core-Kernel';
			package: 'Ring-Deprecated-Core-Containers';
			package: 'Ring-Deprecated-ChunkImporter';
			package: 'Zinc-Resource-Meta-Core';
			package: 'System-Changes';
			package: 'RingChunkImporter';
			package: 'Compression';
			package: 'Monticello';
			package: 'Ring-Deprecated-Monticello';
			package: 'System-Model';
			
			package: 'Network-Kernel';
			package: 'Network-MIME';
			package: 'Network-Protocols';
			package: 'MonticelloRemoteRepositories';
			package: 'Zinc-HTTP';
			package: 'Zinc-FileSystem';
			package: 'Zodiac-Core'.
		spec 
			group: 'Core' with: #('RingChunkImporter' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Ring-Deprecated-Core-Kernel' 'Ring-Deprecated-Core-Containers' 'RingChunkImporter' 'Compression' 'Monticello' 'Ring-Deprecated-Monticello' 'System-Model');
			group: 'RemoteRepositories' with: #( 'Network-Kernel' 'Network-MIME' 'Network-Protocols' 'MonticelloRemoteRepositories' 'Zinc-HTTP' 'Zinc-FileSystem' 'Zodiac-Core' );

			group: 'default' with: #('Core' 'RemoteRepositories' ). ].		

]
