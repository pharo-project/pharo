Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-Monticello-Browser' }
ClyMethod class >> calypsoSystemPluginsMonticelloBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tabActions add: ClyIcebergShowMethodVersionCommand order: 1000.
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: ClyIcebergShowMethodVersionCommand
]
