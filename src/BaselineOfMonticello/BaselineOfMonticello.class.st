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
			package: 'Ring2-Core';
			package: 'Ring2-Compatibility';
			package: 'Ring2-ChunkImporter';
			package: 'Zinc-Resource-Meta-Core';
			package: 'System-Changes';
			package: 'Compression';
			package: 'Monticello';
			package: 'Ring2-Monticello';
			
			package: 'Network-Kernel';
			package: 'Network-MIME';
			package: 'Network-Protocols';
			package: 'MonticelloRemoteRepositories';
			package: 'Zinc-HTTP';
			package: 'Zinc-FileSystem';
			package: 'Zodiac-Core'.
		spec 
			group: 'Core' with: #('Ring2-Core' 'Ring2-ChunkImporter' 'Zinc-Resource-Meta-Core' 'System-Changes' 'Compression' 'Monticello' 'Ring2-Compatibility' 'Ring-Monticello');
			group: 'RemoteRepositories' with: #( 'Network-Kernel' 'Network-MIME' 'Network-Protocols' 'MonticelloRemoteRepositories' 'Zinc-HTTP' 'Zinc-FileSystem' 'Zodiac-Core' );

			group: 'default' with: #('Core' 'RemoteRepositories' ). ].		

]
