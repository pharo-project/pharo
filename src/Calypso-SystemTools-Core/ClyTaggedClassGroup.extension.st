Extension { #name : 'ClyTaggedClassGroup' }

{ #category : '*Calypso-SystemTools-Core' }
ClyTaggedClassGroup class >> systemCommandsPackageCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: SycPackageExtraMenuGroup)
		add: SycDemoteToPackageWithTagCommand order: 10;
		add: SycFileOutPackageCommand order: 20;
		add: SycInspectPackageCommand order: 30;
		add: SycPromotePackageFromTagCommand order: 40;
		add: SycRecompilePackageCommand order: 50
]
