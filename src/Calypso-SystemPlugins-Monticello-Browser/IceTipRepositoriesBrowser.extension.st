Extension { #name : 'IceTipRepositoriesBrowser' }

{ #category : '*Calypso-SystemPlugins-Monticello-Browser' }
IceTipRepositoriesBrowser class >> calypsoCommandsWith: presenter forRootGroup: aCommandGroup [
	<extensionCommands>

	aCommandGroup / self selectionCommandGroupName register:
		ClyBrowseIcebergRepositoryCommand forSpec
]
