"
I'm a model for IcePackage entries.
"
Class {
	#name : #IceTipPackageModel,
	#superclass : #IceTipEntityModel,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #actions }
IceTipPackageModel >> browsePackage [
	<noCache>

	self entity browse
]

{ #category : #actions }
IceTipPackageModel >> browseRepositoryVersion [
	<noCache>
	| snapshot |
	
	snapshot := self entity mcSnapshotInRepository.
	(MCSnapshotBrowser forSnapshot: snapshot)
		showLabelled: 'Snapshot for ', self entity name
]

{ #category : #accessing }
IceTipPackageModel >> descriptionWithDecoration [
	| text |
	
	text := self displayString asText.
	self entity isLoaded
		ifFalse:[ text := text makeAllColor: self colorNotLoaded ].
	self entity isModified 
		ifTrue: [ text := ('*', text) asText makeAllColor: self colorModified ].
	^ text
]

{ #category : #testing }
IceTipPackageModel >> isLoaded [
	^ self entity isLoaded
]

{ #category : #testing }
IceTipPackageModel >> isModified [
	^ self entity isModified
]

{ #category : #actions }
IceTipPackageModel >> load [
	<noCache>

	IceTipStandardAction new 
		repository: self entity repository;
		message: ('Loading package {1}' format: { self entity name });
		onSuccessRepositoryModified;
		execute: [ 
			Iceberg announcer
				suspendAllForRepository: self entity repository
				while: [ self entity load ] ]
]

{ #category : #accessing }
IceTipPackageModel >> name [
	^ self entity name
]

{ #category : #accessing }
IceTipPackageModel >> packageName [
	^ self entity packageName
]

{ #category : #actions }
IceTipPackageModel >> reload [
	<noCache>

	IceTipStandardAction new 
		repository: self entity repository;
		message: ('Reloading package {1}' format: { self entity name }) ;
		onSuccessRepositoryModified;
		execute: [ 
			Iceberg announcer 
				suspendAllForRepository: self entity repository 
				while: [ self entity reload ] ]
]

{ #category : #actions }
IceTipPackageModel >> remove [
	<noCache>

	IceTipStandardAction new 
		repository: self entity repository;
		message: ('Removing package {1}' format: { self entity name });
		onSuccessRepositoryModified; 
		execute: [ 
			Iceberg announcer
				suspendAllForRepository: self entity repository
				while: [ self entity remove ] ]
]

{ #category : #accessing }
IceTipPackageModel >> status [
	"entity could be a repository or a package"
	[
		self entity repository isMissing ifTrue: [ ^ self toErrorMessage: 'Local repository missing' ].
		self entity isLoaded ifFalse: [ ^ 'Not loaded' ].
		self entity isModified ifTrue: [ ^ 'Uncommited changes' ].
		^ 'Up to date' ] 
	on: Error do: [ :e | 
		^ self toErrorMessage: e description ]
]

{ #category : #accessing }
IceTipPackageModel >> statusWithDecoration [
	| text |
	
	text := self status asText.
	self entity isLoaded 
		ifFalse: [ text := text makeAllColor: self colorNotLoaded ].
	^ text
]

{ #category : #actions }
IceTipPackageModel >> unload [
	<noCache>

	IceTipStandardAction new 
		repository: self entity repository;
		message: ('Unloading package {1}' format: { self entity name });
		onSuccessRepositoryModified; 
		execute: [ 
			Iceberg announcer
				suspendAllForRepository: self entity repository
				while: [ self entity unload ] ]
]
