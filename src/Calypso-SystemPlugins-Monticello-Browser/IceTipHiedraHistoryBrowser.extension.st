Extension { #name : 'IceTipHiedraHistoryBrowser' }

{ #category : '*Calypso-SystemPlugins-Monticello-Browser' }
IceTipHiedraHistoryBrowser class >> calypsoCommandsWith: presenter forRootGroup: aCommandGroup [
	<extensionCommands>

	aCommandGroup / self selectionCommandGroupName register:
		ClyBrowseIcebergCommitCommand forSpec
]
