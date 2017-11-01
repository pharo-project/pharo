"
This baseline is used to store information about Monticello packages. It is not directly used for the packages loading during bootstrap.
"
Class {
	#name : #BaselineOfMonticello,
	#superclass : #BaselineOf,
	#category : 'BaselineOfMonticello'
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
			package: 'Ring-Core-Kernel';
			package: 'Ring-Core-Containers';
			package: 'RingChunkImporter';
			package: 'Zinc-Resource-Meta-Core';
			package: 'System-Changes';
			package: 'RingChunkImporter';
			package: 'Compression';
			package: 'Monticello';
			package: 'Ring-Monticello';
			
			package: 'Network-Kernel';
			package: 'Network-MIME';
			package: 'Network-Url';
			package: 'Network-Protocols';
			package: 'MonticelloRemoteRepositories';
			package: 'Zinc-HTTP';
			package: 'Zinc-FileSystem';
			package: 'Zodiac-Core'.
		spec 
			group: 'Core' with: #('RingChunkImporter' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Ring-Core-Kernel' 'Ring-Core-Containers' 'RingChunkImporter' 'Compression' 'Monticello' 'Ring-Monticello');
			group: 'RemoteRepositories' with: #( 'Network-Kernel' 'Network-MIME' 'Network-Url' 'Network-Protocols' 'MonticelloRemoteRepositories' 'Zinc-HTTP' 'Zinc-FileSystem' 'Zodiac-Core' );

			group: 'default' with: #('Core' 'RemoteRepositories' ). ].		

]
