Extension { #name : 'Package' }

{ #category : '*SystemCommands-PackageCommands' }
Package class >> systemCommandsPackageCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: SycOpenExtraInPackageMenuCommand order: 10200
]
