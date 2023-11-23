Extension { #name : 'SycRenamePackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenamePackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyFullBrowserPackageContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenamePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyFullBrowserPackageContext
]
