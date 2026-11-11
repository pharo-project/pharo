Extension { #name : 'ClyTaggedClassGroup' }

{ #category : '*SystemCommands-PackageCommands' }
ClyTaggedClassGroup class >> systemCommandsPackageCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenExtraInPackageMenuCommand order: 10200
]
