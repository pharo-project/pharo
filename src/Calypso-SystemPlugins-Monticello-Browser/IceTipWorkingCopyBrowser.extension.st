Extension { #name : 'IceTipWorkingCopyBrowser' }

{ #category : '*Calypso-SystemPlugins-Monticello-Browser' }
IceTipWorkingCopyBrowser class >> calypsoCommandsWith: presenter forRootGroup: aCommandGroup [

	<extensionCommands>
	aCommandGroup / self selectionCommandGroupName register:
		(ClyBrowseIcebergPackageCommand forSpecContext: presenter)
]
